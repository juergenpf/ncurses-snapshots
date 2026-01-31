# You may change the following two variables according to your environment
$NC_DISTRO_NAME="Ubuntu"
$NC_SRC_PATTERN="\home\@USER@\src\ncurses"

$NC_INIPATH=$Env:PATH

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
    
    if (Test-Path -Path $ConfigLog -PathType Leaf) {
        $Legacy=Select-String -Path $ConfigLog -Pattern "--disable-widec" -Quiet
    }
    Push-Location -Path "$target"
    $Env:PATH="$inst\bin;$NC_INIPATH"
    $Env:TERM="ms-terminal"
    $Env:TERMINFO="$inst\share\terminfo"
    if (!$Legacy) {
	[Console]::OutputEncoding = [System.Text.Encoding]::GetEncoding(65001)
        $Env:NC_WINCP="65001"
        $Env:NC_WIN_CTYPE="German_Germany.UTF-8"
    } else {
	[Console]::OutputEncoding = [System.Text.Encoding]::GetEncoding(1252)
        $Env:NC_WINCP="1252"
        $Env:NC_WIN_CTYPE="German_Germany.1252"
    }
    if ($Trace) {
        $Env:NCURSES_TRACE=8191
    }
    Write-Host "NC_WIN_CTYPE: $Env:NC_WIN_CTYPE"
    Write-Host "NC_WINCP: $Env:NC_WINCP"
    Write-Host "TERM: $Env:TERM"
    Write-Host "TERMINFO: $Env:TERMINFO"
    if ($Legacy) {
        cd $ENV:USERPROFILE
        Start-Process cmd -ArgumentList "/K","PUSHD $Target && chcp $Env:NC_WINCP"
    }
}

function Start-MinGWDebug {
    param(
        [string]$Program
    )
    $registryPath = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\MSYS2 64bit_is1"
    $installDir = (Get-ItemProperty -Path $registryPath -ErrorAction SilentlyContinue).InstallLocation

    if (-not $installDir) {
        if (Test-Path "${Env:SystemDrive}\msys64") { $installDir = "${Env:SystemDrive}\msys64" }
    }

    if ($installDir) {
        $gdbPath = Join-Path (Join-Path (Join-Path $installDir "mingw64") "bin") "gdb.exe"
        if (Test-Path $gdbPath -PathType Leaf) {
       	    & $gdbPath $Program
        } else {
	    Write-Error "gdb.exe not found"
	}
    }
}

Set-Alias nct Test-NCurses
Set-Alias ncdbg Start-MinGWDebug
