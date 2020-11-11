# Purty fork

Very opinionated fork of [Purty](https://gitlab.com/joneshf/purty). A source code formatter for PureScript

## Installation

### Sources

```
git clone https://github.com/Zelenya/purty.git
cd purty
```

Then, you need to build it using Stack
```
stack build
```

Probably, you can find your executables like this
```
echo "$(stack path --local-install-root)/bin/purty"
```

## Usage

### VSCode

0. Get VSCode
1. Enable `vscode-purty` extension
2. Open the extension's settings and set `Purty: Path To Purty`
3. Open settings and enable `Editor: Format On Save`

### Manual (for some reason)

To format the module and write it back to the same file:
```
purty --write src/Main.purs
```

To format a lot
```
for i in **/*.purs; do purty --write $i; done
```

## Format

```
import Prelude

data Data
  = Joy
  | Meh

derive instance dataEq ∷ Eq Data
derive instance dataOrd ∷ Ord Data

type ThisLine =
  { random ∷ Data }

example ∷ ∀ a. a -> Data
example = do
  let dontJump = "toNewLine"
  pure Joy
  where
    whereHasIdentation = true

    withType ∷ Int -> Int
    withType 1 = 3
    withType _ = 42
```
