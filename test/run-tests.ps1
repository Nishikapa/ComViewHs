# comview のエンドツーエンドテスト:
# テスト用 CFB ファイルを生成し、ツリー表示の出力を期待値と比較する。
# 実行方法: pwsh .\test\run-tests.ps1
$ErrorActionPreference = 'Stop'
[Console]::OutputEncoding = [Text.Encoding]::UTF8

$repoRoot = Split-Path $PSScriptRoot -Parent
$outDir   = Join-Path $PSScriptRoot 'out'

$cases = @(
    @{ Name = 'single sector';    Gen = 'make-testfile1.ps1'; Cfb = 'test1.cfb'; Expected = 'expected1.txt' }
    @{ Name = 'multi-sector dir'; Gen = 'make-testfile2.ps1'; Cfb = 'test2.cfb'; Expected = 'expected2.txt' }
)

Push-Location $repoRoot
try {
    $failed = 0
    foreach ($case in $cases) {
        $cfb = Join-Path $outDir $case.Cfb
        & (Join-Path $PSScriptRoot $case.Gen) -OutFile $cfb

        $actual   = cabal run -v0 comview.hs -- $cfb
        if ($LASTEXITCODE -ne 0) { throw "comview failed on $($case.Cfb)" }
        $expected = Get-Content (Join-Path $PSScriptRoot $case.Expected) -Encoding UTF8

        if (Compare-Object $expected $actual) {
            $failed++
            Write-Host "FAIL: $($case.Name)" -ForegroundColor Red
            Write-Host '--- expected ---'; $expected
            Write-Host '--- actual ---';   $actual
        }
        else {
            Write-Host "PASS: $($case.Name)" -ForegroundColor Green
        }
    }
    if ($failed -gt 0) { exit 1 }
    Write-Host 'All tests passed.'
}
finally {
    Pop-Location
}
