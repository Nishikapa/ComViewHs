# ディレクトリストリームが 2 セクタにまたがる MS-CFB v3 ファイルを生成する (FAT チェーン追跡の確認用)
param([string]$OutFile = "$PSScriptRoot\out\test2.cfb")

$b = New-Object byte[] 2048

function W16([int]$off, [uint16]$v) { [BitConverter]::GetBytes($v).CopyTo($script:b, $off) }
function W32([int]$off, [uint32]$v) { [BitConverter]::GetBytes($v).CopyTo($script:b, $off) }

$FREE = [uint32]::MaxValue       # 0xFFFFFFFF FREESECT / NOSTREAM
$EOC  = [uint32]::MaxValue - 1   # 0xFFFFFFFE ENDOFCHAIN
$FATS = [uint32]::MaxValue - 2   # 0xFFFFFFFD FATSECT

# --- ヘッダ ---
[byte[]](0xD0,0xCF,0x11,0xE0,0xA1,0xB1,0x1A,0xE1) | ForEach-Object -Begin { $i = 0 } -Process { $b[$i++] = $_ }
W16 24 0x003E; W16 26 0x0003; W16 28 0xFFFE; W16 30 9; W16 32 6
W32 44 1           # FAT セクタ数
W32 48 1           # ディレクトリ開始セクタ = 1
W32 56 4096
W32 60 $EOC; W32 68 $EOC
W32 76 0           # DIFAT[0] -> FAT はセクタ 0
for ($i = 1; $i -lt 109; $i++) { W32 (76 + 4 * $i) $FREE }

# --- FAT (セクタ 0): ディレクトリチェーンは 1 -> 2 -> 終端 ---
W32 512 $FATS
W32 516 2
W32 520 $EOC
for ($i = 3; $i -lt 128; $i++) { W32 (512 + 4 * $i) $FREE }

# --- ディレクトリ (セクタ 1, 2): 8 エントリ ---
function WriteEntry([int]$idx, [string]$name, [byte]$type, [uint32]$left, [uint32]$right, [uint32]$child) {
    $base = 1024 + 128 * $idx
    $nameBytes = [Text.Encoding]::Unicode.GetBytes($name)
    $nameBytes.CopyTo($script:b, $base)
    W16 ($base + 64) ($nameBytes.Length + 2)
    $script:b[$base + 66] = $type
    W32 ($base + 68) $left
    W32 ($base + 72) $right
    W32 ($base + 76) $child
    W32 ($base + 116) $script:EOC
}

# ルート直下: A, B, C。B はストレージで配下に D, E, F, G
WriteEntry 0 'Root Entry' 5 $FREE $FREE 1
WriteEntry 1 'A' 2 $FREE 2     $FREE
WriteEntry 2 'B' 1 $FREE 3     5        # child -> E (ID 5)。兄弟は left/right でたどる
WriteEntry 3 'C' 2 $FREE $FREE $FREE
WriteEntry 4 'D' 2 $FREE $FREE $FREE
WriteEntry 5 'E' 2 4     6     $FREE    # left = D, right = F
WriteEntry 6 'F' 2 $FREE 7     $FREE    # right = G
WriteEntry 7 'G' 2 $FREE $FREE $FREE

New-Item -ItemType Directory -Force (Split-Path $OutFile -Parent) | Out-Null
[IO.File]::WriteAllBytes($OutFile, $b)
