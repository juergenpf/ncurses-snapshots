# $Id: MKkeyname.awk,v 1.4 1996/07/30 23:37:27 tom Exp $
BEGIN {
	print ""
	print "#include <config.h>"
	print "#include <stdlib.h>"
	print "#include <term.h>"
	print "#include <curses.h>"
	print ""
	print "struct kn {"
	print "\tchar *name;"
	print "\tint code;"
	print "};"
	print ""
	print "const struct kn key_names[] = {"
}

{printf "\t{\"%s\", %s,},\n", $1, $2;}

END {
	print "};"
	print ""
	print "const char *keyname(int c)"
	print "{"
	print "int i, size = sizeof(key_names)/sizeof(struct kn);"
	print ""
	print "\tfor (i = 0; i < size; i++) {"
	print "\t\tif (key_names[i].code == c) return key_names[i].name;"
	print "\t}"
	print "\treturn NULL;"
	print "}"
	print "" 
}