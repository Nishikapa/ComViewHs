# 最小構成の MS-CFB v3 ファイルを生成する: ヘッダ + FAT セクタ 1 個 + ディレクトリセクタ 1 個
param([string]$OutFile = "$PSScriptRoot\out\test1.cfb")

$b = New-Object byte[] 1536

function W16([int]$off, [uint16]$v) { [BitConverter]::GetBytes($v).CopyTo($script:b, $off) }
function W32([int]$off, [uint32]$v) { [BitConverter]::GetBytes($v).CopyTo($script:b, $off) }

$FREE = [uint32]::MaxValue       # 0xFFFFFFFF FREESECT / NOSTREAM
$EOC  = [uint32]::MaxValue - 1   # 0xFFFFFFFE ENDOFCHAIN
$FATS = [uint32]::MaxValue - 2   # 0xFFFFFFFD FATSECT

# --- ヘッダ ---
[byte[]](0xD0,0xCF,0x11,0xE0,0xA1,0xB1,0x1A,0xE1) | ForEach-Object -Begin { $i = 0 } -Process { $b[$i++] = $_ }
W16 24 0x003E      # minor version
W16 26 0x0003      # major version (3 = 512 バイトセクタ)
W16 28 0xFFFE      # byte order
W16 30 9           # sector shift (2^9 = 512)
W16 32 6           # mini sector shift
W32 44 1           # FAT セクタ数
W32 48 1           # ディレクトリ開始セクタ = 1
W32 56 4096        # ミニストリーム閾値
W32 60 $EOC        # ミニ FAT 開始セクタ (なし)
W32 68 $EOC        # DIFAT 開始セクタ (なし)
W32 76 0           # DIFAT[0] -> FAT はセクタ 0
for ($i = 1; $i -lt 109; $i++) { W32 (76 + 4 * $i) $FREE }

# --- FAT (セクタ 0) ---
W32 512 $FATS               # セクタ 0 = FATSECT
W32 516 $EOC                # セクタ 1 (ディレクトリ) はチェーン終端
for ($i = 2; $i -lt 128; $i++) { W32 (512 + 4 * $i) $FREE }

# --- ディレクトリ (セクタ 1) ---
function WriteEntry([int]$idx, [string]$name, [byte]$type, [uint32]$left, [uint32]$right, [uint32]$child) {
    $base = 1024 + 128 * $idx
    $nameBytes = [Text.Encoding]::Unicode.GetBytes($name)
    $nameBytes.CopyTo($script:b, $base)
    W16 ($base + 64) ($nameBytes.Length + 2)   # 終端 NUL 込みの長さ
    $script:b[$base + 66] = $type              # 1=storage, 2=stream, 5=root
    W32 ($base + 68) $left
    W32 ($base + 72) $right
    W32 ($base + 76) $child
    W32 ($base + 116) $script:EOC              # 開始セクタ (ストリーム実体なし)
}

WriteEntry 0 'Root Entry' 5 $FREE $FREE 1
WriteEntry 1 'StreamA'    2 $FREE 2     $FREE
WriteEntry 2 'Storage1'   1 $FREE $FREE 3
WriteEntry 3 'StreamB'    2 $FREE $FREE $FREE

New-Item -ItemType Directory -Force (Split-Path $OutFile -Parent) | Out-Null
[IO.File]::WriteAllBytes($OutFile, $b)
