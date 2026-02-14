#------------------------------------------------------------------------------
## Copyright 2020 Thomas E. Dickey                                           ##
## Copyright 2008-2011,2012 Free Software Foundation, Inc.                   ##
##                                                                           ##
## Permission is hereby granted, free of charge, to any person obtaining a   ##
## copy of this software and associated documentation files (the             ##
## "Software"), to deal in the Software without restriction, including       ##
## without limitation the rights to use, copy, modify, merge, publish,       ##
## distribute, distribute with modifications, sublicense, and/or sell copies ##
## of the Software, and to permit persons to whom the Software is furnished  ##
## to do so, subject to the following conditions:                            ##
##                                                                           ##
## The above copyright notice and this permission notice shall be included   ##
## in all copies or substantial portions of the Software.                    ##
##                                                                           ##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS   ##
## OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF                ##
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN ##
## NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,       ##
## DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR     ##
## OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE ##
## USE OR OTHER DEALINGS IN THE SOFTWARE.                                    ##
##                                                                           ##
## Except as contained in this notice, the name(s) of the above copyright    ##
## holders shall not be used in advertising or otherwise to promote the      ##
## sale, use or other dealings in this Software without prior written        ##
## authorization.                                                            ##
#------------------------------------------------------------------------------
## $Id$
## Author: Juergen Pfeifer
#------------------------------------------------------------------------------
# You may change the following variables according to your environment
[string]$DEFAULT_WSL_DISTRO="Ubuntu"
[string]$DEFAULT_UTF8_LOCALE="German_Germany.UTF-8"
[int]$DEFAULT_ASCII_CODEPAGE=1252
[string]$DEFAULT_ASCII_LOCALE="German_Germany.1252"
[string]$NC_SRC_PATTERN="\home\@USER@\src\ncurses"

# Please leave the rest of the code unchanged, unless you know what you are doing. 
# The functions defined below rely on the above variables and may not work correctly 
# if you change them without understanding the implications.
$NC_INIPATH=$Env:PATH

function Get-MinGWGDBPath {
    $registryPath = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\MSYS2 64bit_is1"
    $installDir = (Get-ItemProperty -Path $registryPath -ErrorAction SilentlyContinue).InstallLocation

    if (-not $installDir) {
        if (Test-Path "${Env:SystemDrive}\msys64") { $installDir = "${Env:SystemDrive}\msys64" }
    }

    if ($installDir) {
        $gdbPath = Join-Path (Join-Path (Join-Path $installDir "mingw64") "bin") "gdb.exe"
        if (Test-Path $gdbPath -PathType Leaf) {
       	    return $gdbPath
        } else {
        Write-Error "gdb.exe not found"
    }
    }
    return $null
}

function Test-NCurses {
    [CmdletBinding()]
    param(
        [Switch]$Trace,
        [switch]$SameSession
    )
    [string]$User=${Env:USERNAME}.ToLowerInvariant()
    [string]$src=$NC_SRC_PATTERN -replace "@USER@",$User
    [string]$NCSRC="\\wsl$\${DEFAULT_WSL_DISTRO}${src}"
    [string]$inst="${NCSRC}\inst\mingw64"
    [string]$Target="$NCSRC\build\test"
    [bool]$Legacy=$false
    [string]$ConfigLog=(Join-Path (Join-Path $NCSRC "build") "config.log")
    [int]$DefaultCP=65001
    [string]$DefaultLocale=$DEFAULT_UTF8_LOCALE

    if (-not (Test-Path -Path $target -PathType Container)) {
        Write-Error "Test directory not found: $target"
        return
    }
    if (Test-Path -Path $ConfigLog -PathType Leaf) {
        $Legacy=Select-String -Path $ConfigLog -Pattern "--disable-widec" -Quiet
    }
    if ($Legacy) {
        $DefaultCP=$DEFAULT_ASCII_CODEPAGE
        $DefaultLocale=$DEFAULT_ASCII_LOCALE
    }
    if (!$Legacy -or ($Legacy -and $SameSession)) {
        Push-Location -Path "$target"
        $Env:PATH="$inst\bin;$NC_INIPATH"
        $Env:TERM="ms-terminal"
        $Env:TERMINFO="$inst\share\terminfo"
        if ($Trace) {
            $Env:NCURSES_TRACE=8191
        } else {
            $Env:NCURSES_TRACE=$null
        }

	    [Console]::OutputEncoding = [System.Text.Encoding]::GetEncoding($DefaultCP)
        $Env:NC_WINCP=$DefaultCP
        $Env:NC_WIN_CTYPE=$DefaultLocale   
        Write-Host "NC_WIN_CTYPE=$Env:NC_WIN_CTYPE"
        Write-Host "NC_WINCP=$Env:NC_WINCP"
        Write-Host "TERM=$Env:TERM"
        Write-Host "TERMINFO=$Env:TERMINFO"
        Write-Host "NC_TRACE=${Env:NCURSES_TRACE}"
    }
    
    if ($Legacy) {
        if (-not $SameSession) {
            $passedParams = ($PSBoundParameters.GetEnumerator() | ForEach-Object {
                if ($_.Value -is [switch]) {
                    if ($_.Value) { "-$($_.Key)" }
                } else {
                    "-$($_.Key) '$($_.Value)'"
                }
            } | Where-Object { $_ }) -join ' '
            Start-Process (Join-Path $PSHOME "pwsh.exe") -ArgumentList "-NoExit", "-Command","Test-NCurses -SameSession $passedParams"
        }
    }
}

function Start-MinGWDebug {
    param(
        [string]$Program
    )
    $gdbPath = Get-MinGWGDBPath
    if ($gdbPath) {
        & $gdbPath $Program
    }   
    else {
	    Write-Error "gdb.exe not found"
	}
}

Set-Alias nct Test-NCurses
Set-Alias ncdbg Start-MinGWDebug
