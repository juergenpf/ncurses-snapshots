/****************************************************************************
 * Copyright 2020-2024,2025 Thomas E. Dickey                                *
 * Copyright 1998-2009,2010 Free Software Foundation, Inc.                  *
 *                                                                          *
 * Permission is hereby granted, free of charge, to any person obtaining a  *
 * copy of this software and associated documentation files (the            *
 * "Software"), to deal in the Software without restriction, including      *
 * without limitation the rights to use, copy, modify, merge, publish,      *
 * distribute, distribute with modifications, sublicense, and/or sell       *
 * copies of the Software, and to permit persons to whom the Software is    *
 * furnished to do so, subject to the following conditions:                 *
 *                                                                          *
 * The above copyright notice and this permission notice shall be included  *
 * in all copies or substantial portions of the Software.                   *
 *                                                                          *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  *
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               *
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   *
 * IN NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,   *
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR    *
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR    *
 * THE USE OR OTHER DEALINGS IN THE SOFTWARE.                               *
 *                                                                          *
 * Except as contained in this notice, the name(s) of the above copyright   *
 * holders shall not be used in advertising or otherwise to promote the     *
 * sale, use or other dealings in this Software without prior written       *
 * authorization.                                                           *
 ****************************************************************************/

/****************************************************************************
 *  Author: Juergen Pfeifer                                                 *
 *     and: Thomas E. Dickey                                                *
 ****************************************************************************/

#include <curses.priv.h>

MODULE_ID("$Id: lib_win32util.c,v 1.7 2025/06/28 16:58:13 tom Exp $")

#ifdef _NC_WINDOWS_NATIVE

#if HAVE_GETTIMEOFDAY == 2
#define JAN1970 116444736000000000LL /* the value for 01/01/1970 00:00 */

NCURSES_EXPORT(int)
_nc_gettimeofday(struct timeval *tv, void *tz GCC_UNUSED)
{
	union
	{
		FILETIME ft;
		long long since1601; /* time since 1 Jan 1601 in 100ns units */
	} data;

	GetSystemTimeAsFileTime(&data.ft);
	tv->tv_usec = (long)((data.since1601 / 10LL) % 1000000LL);
	tv->tv_sec = (long)((data.since1601 - JAN1970) / 10000000LL);
	return (0);
}
#endif // HAVE_GETTIMEOFDAY == 2

#if USE_WIDEC_SUPPORT
// To avoid unpredictable interferences with the various C runtimes on Windows,
// we use O_BINARY mode on the input and output file handles. This requires, that
// we handle the UTF-8 encoding and decoding ourselves.
NCURSES_EXPORT(size_t)
_nc_wchar_to_utf8(wchar_t wc, char utf8[UTF8_MAX_BYTES])
{
	wchar_t wstr[2] = {wc, L'\0'};
	int result;

	result = WideCharToMultiByte(CP_UTF8, 0, wstr, 1, utf8, 4, NULL, NULL);
	if (result > 0)
		return (size_t)result;
	else
		return 0; // signals error
}

typedef struct
{
	unsigned char buffer[UTF8_MAX_BYTES]; /* Buffer for incomplete UTF-8 sequence */
	size_t length;			      /* Current length of buffer */
	mbstate_t state;		      /* Multibyte conversion state */
} utf8_input_buffer_t;

static utf8_input_buffer_t utf8_buffer = {0};

/*
 * Convert Unicode codepoint to Windows wchar_t (UTF-16)
 * Handles surrogate pairs for codepoints > 0xFFFF
 * Returns: 1 for BMP characters, 2 for surrogate pairs, -1 for invalid
 */
static int
codepoint_to_wchar(uint32_t codepoint, wchar_t *wch)
{
	if (codepoint <= 0xFFFF)
	{
		/* Basic Multilingual Plane - direct conversion */
		if (codepoint >= 0xD800 && codepoint <= 0xDFFF)
		{
			/* Invalid: surrogate range should not appear in UTF-8 */
			return -1;
		}
		wch[0] = (wchar_t)codepoint;
		return 1;
	}
	else if (codepoint <= 0x10FFFF)
	{
		/* Supplementary planes - needs surrogate pair for Windows */
		/* Convert to UTF-16 surrogate pair */
		uint32_t code = codepoint - 0x10000;
		wch[0] = (wchar_t)(0xD800 + (code >> 10));   /* High surrogate */
		wch[1] = (wchar_t)(0xDC00 + (code & 0x3FF)); /* Low surrogate */
		return 2;
	}
	else
	{
		/* Invalid codepoint */
		return -1;
	}
}

/*
 * Enhanced UTF-8 decoder with overlong sequence detection
 * Returns Unicode codepoint or -1 for invalid sequences
 */
static int
decode_utf8_simple(const unsigned char *bytes, size_t length, uint32_t *codepoint)
{
	if (length == 0)
		return 0;

	unsigned char first = bytes[0];
	int expected_bytes;
	uint32_t cp = 0;
	uint32_t min_value; /* Minimum valid codepoint for this sequence length */

	/* Determine expected number of bytes from first byte */
	if ((first & 0x80) == 0)
	{
		/* ASCII (0xxxxxxx) */
		expected_bytes = 1;
		cp = first;
		min_value = 0x00;
	}
	else if ((first & 0xE0) == 0xC0)
	{
		/* 2-byte sequence (110xxxxx) */
		expected_bytes = 2;
		cp = first & 0x1F;
		min_value = 0x80;
	}
	else if ((first & 0xF0) == 0xE0)
	{
		/* 3-byte sequence (1110xxxx) */
		expected_bytes = 3;
		cp = first & 0x0F;
		min_value = 0x800;
	}
	else if ((first & 0xF8) == 0xF0)
	{
		/* 4-byte sequence (11110xxx) */
		expected_bytes = 4;
		cp = first & 0x07;
		min_value = 0x10000;
	}
	else
	{
		/* Invalid UTF-8 start byte */
		return -1;
	}

	/* Check if we have enough bytes */
	if ((int)length < expected_bytes)
	{
		return 0; /* Need more bytes */
	}

	/* Process continuation bytes */
	for (int i = 1; i < expected_bytes; i++)
	{
		if ((bytes[i] & 0xC0) != 0x80)
		{
			return -1; /* Invalid continuation byte */
		}
		cp = (cp << 6) | (bytes[i] & 0x3F);
	}

	/* Check for overlong sequences - reject if codepoint could be
	 * encoded in fewer bytes */
	if (cp < min_value)
	{
		return -1; /* Overlong encoding */
	}

	/* Check for maximum valid Unicode codepoint */
	if (cp > 0x10FFFF)
	{
		return -1; /* Beyond valid Unicode range */
	}

	*codepoint = cp;
	return expected_bytes;
}

/*
 * Assemble incoming UTF-8 bytes into complete characters
 * Returns:
 *  > 0: Complete character assembled (Unicode codepoint or special value for surrogates)
 *  0:   Need more bytes
 * -1:   Invalid UTF-8 sequence (reset buffer)
 * -2:   Surrogate pair needed but wch buffer too small (use extended function)
 */
NCURSES_EXPORT(int)
_nc_assemble_utf8_input(unsigned char byte, wchar_t *wch)
{
	if (utf8_buffer.length >= UTF8_MAX_BYTES)
	{
		memset(&utf8_buffer, 0, sizeof(utf8_buffer));
		return -1;
	}

	utf8_buffer.buffer[utf8_buffer.length++] = byte;

	/* Unified UTF-8 decoding for both widec and non-widec builds */
	uint32_t codepoint;
	int consumed = decode_utf8_simple(utf8_buffer.buffer,
					  utf8_buffer.length,
					  &codepoint);
	if (consumed > 0)
	{
		memset(&utf8_buffer, 0, sizeof(utf8_buffer));
		codepoint_to_wchar(codepoint, wch);
		/* For Windows, handle potential surrogate pairs */
		if (codepoint <= 0xFFFF)
		{
			/* BMP character - direct conversion is safe */
			*wch = (wchar_t)codepoint;
			return (int)codepoint;
		}
		else
		{
			/* Supplementary plane - would need surrogate pair on Windows */
			/* For terminal input, we'll use the codepoint value directly */
			/* The calling code should handle the Windows-specific conversion */
			*wch = 0xFFFD; /* Replacement character as fallback */
			/* Return the original codepoint for proper handling upstream */
			return (int)codepoint;
		}
	}
	else if (consumed == 0)
	{
		return 0; /* Need more bytes */
	}
	else
	{
		memset(&utf8_buffer, 0, sizeof(utf8_buffer));
		return -1; /* Invalid sequence */
	}
}

#define mk_wcwidth(ucs)          _nc_wcwidth(ucs)
#define mk_wcswidth(pwcs, n)     _nc_wcswidth(pwcs, n)
#define mk_wcwidth_cjk(ucs)      _nc_wcwidth_cjk(ucs)
#define mk_wcswidth_cjk(pwcs, n) _nc_wcswidth_cjk(pwcs, n)

#include <stddef.h>

typedef enum {
    WcUnknown = 0
    ,WcSoftHyphen = 1		/* soft-hyphen is spacing, e.g., Latin-1 */
    ,WcPrivateFullwidth = 2	/* private-use codes can be fullwidth in CJK */
    ,WcEmojiFullwidth = 4	/* Emojis are fullwidth */
} WcModes;

NCURSES_EXPORT(int) mk_wcwidth_init(int);
NCURSES_EXPORT(int) mk_wcwidth(uint32_t);
NCURSES_EXPORT(int) mk_wcswidth(const uint32_t *, size_t);
NCURSES_EXPORT(int) mk_wcwidth_cjk(uint32_t);
NCURSES_EXPORT(int) mk_wcswidth_cjk(const uint32_t *, size_t);
NCURSES_EXPORT(int) mk_is_emoji(wchar_t ucs);

#include <wcwidth.h>
#else
void _nc_empty_wcwidth(void);
void
_nc_empty_wcwidth(void)
{
}
#endif // USE_WIDEC_SUPPORT

#endif // _NC_WINDOWS_NATIVE
