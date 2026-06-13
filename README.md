# ComViewHs

複合ファイル (Compound File Binary Format / MS-CFB、いわゆる OLE2 構造化ストレージ) の内部ディレクトリ構造をツリー表示する Haskell 製のコマンドラインツールです。

旧形式の Office 文書 (.xls / .doc / .ppt) や .msi、.msg などが複合ファイル形式です。

## 使い方

```
comview <複合ファイルのパス>
```

出力例:

```
Root Entry
┣ Workbook
┣ ﻿SummaryInformation
┗ ﻿DocumentSummaryInformation
```

ストレージ (フォルダに相当) の下のエントリは字下げして表示されます。

## ビルド

依存パッケージは `binary` / `bytestring` / `text` / `uuid` です。

ソース先頭に cabal スクリプトヘッダが付いているので、cabal があればそのまま実行できます:

```
cabal run comview.hs -- sample.xls
```

実行ファイルを作る場合は、依存パッケージが見える環境を作ってから GHC でコンパイルします:

```
cabal install --lib uuid text binary bytestring --package-env . ^
  --constraint="text installed" --constraint="binary installed" --constraint="bytestring installed"
ghc comview.hs
```

## 処理の流れ

[comview.hs](comview.hs) は MS-CFB 仕様に沿って次の順で読み取ります。

1. **ヘッダ** — ファイル先頭 512 バイト。シグネチャ、セクタサイズ、各構造の開始セクタ番号と、DIFAT の先頭 109 エントリを持つ (`getCompoundFileHeader`)
2. **DIFAT** — ヘッダ内の 109 エントリに、DIFAT セクタのチェーンをたどった分を連結する (`allDIFAT`)。DIFAT は「FAT がどのセクタにあるか」の一覧
3. **FAT** — DIFAT が指す FAT セクタをすべて連結し、ファイル全体のセクタチェーン表を得る (`getFATs`)
4. **ディレクトリストリーム** — ヘッダの開始セクタから FAT のチェーンをたどってストリームを読み (`getStreamData`)、128 バイトごとの `DirectoryEntry` (名前は UTF-16LE) の列にパースする
5. **ツリー表示** — ルートエントリ (ID 0) から、`childID` で 1 階層下り、`leftSiblingID` / `rightSiblingID` の二分木を中順にたどって同じ階層のエントリを名前順に列挙し (`siblingDirectoryEntryIds`)、罫線付きで出力する (`printDirectoryTree`)

## テスト

```
pwsh .\test\run-tests.ps1
```

[test/](test/) のスクリプトが最小構成の複合ファイル (単一セクタ構成と、ディレクトリストリームが 2 セクタにまたがる構成) を生成し、`cabal run` でツリー表示した結果を期待値 (`expected*.txt`) と比較します。

## 制限事項

- セクタサイズ 512 バイトのバージョン 3 形式のみ対応 (4096 バイトセクタのバージョン 4 は非対応)
- 表示するのはディレクトリ構造 (エントリ名) のみで、ストリームの中身は読み出しません
- ミニ FAT / ミニストリームは使用しません

## 参考資料

- [MS-CFB]: Compound File Binary File Format
  - https://learn.microsoft.com/openspecs/windows_protocols/ms-cfb/
  - https://winprotocoldoc.blob.core.windows.net/productionwindowsarchives/MS-CFB/[MS-CFB].pdf
