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
# You may change the following two variables according to your environment
$NC_DISTRO_NAME="Ubuntu"
$NC_SRC_PATTERN="\home\@USER@\src\ncurses"

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
    param(
        [Switch]$Trace
    )
    [string]$User=${Env:USERNAME}.ToLowerInvariant()
    [string]$src=$NC_SRC_PATTERN -replace "@USER@",$User
    [string]$NCSRC="\\wsl$\${NC_DISTRO_NAME}${src}"
    [string]$inst="${NCSRC}\inst\mingw64"
    [string]$Target="$NCSRC\build\test"
    [bool]$Legacy=$false
    [string]$ConfigLog=(Join-Path (Join-Path $NCSRC "build") "config.log")
    [int]$DefaultCP=65001
    [string]$gdb=Get-MinGWGDBPath

    if (-not (Test-Path -Path $target -PathType Container)) {
        Write-Error "Test directory not found: $target"
        return
    }
    if (Test-Path -Path $ConfigLog -PathType Leaf) {
        $Legacy=Select-String -Path $ConfigLog -Pattern "--disable-widec" -Quiet
    }
    if ($Legacy) {
        $DefaultCP=1252
    }
    Push-Location -Path "$target"
    $Env:PATH="$inst\bin;$NC_INIPATH"
    $Env:TERM="ms-terminal"
    $Env:TERMINFO="$inst\share\terminfo"
    if ($Trace) {
        $Env:NCURSES_TRACE=8191
    }
   if (!$Legacy) {
	    [Console]::OutputEncoding = [System.Text.Encoding]::GetEncoding(65001)
    } else {
        Write-Host "Separate Terminal session will be started with these settings:"
        Write-Host "NCDBG=$gdb"
    }
    Write-Host "TERM=$Env:TERM"
    Write-Host "CodePage=$DefaultCP"
    Write-Host "TERMINFO=$Env:TERMINFO"
    if ($Legacy) {
        Pop-Location
        Set-Location $ENV:USERPROFILE
        Start-Process cmd.exe -ArgumentList "/K","PUSHD $Target && SET NCDBG=$gdb SET NC_WINCP=$DefaultCP && SET NC_WIN_CTYPE=$DefaultLocale && chcp $DefaultCP"
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
