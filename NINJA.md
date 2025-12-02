# Karel Ninja Build System

Ninja ist ein schnelles Build-System, optimal für große Projekte. Karel unterstützt Ninja-Builds mit automatischer Konfiguration.

## Installation

### Ninja installieren

```bash
# macOS
brew install ninja

# Ubuntu/Debian
sudo apt-get install ninja-build

# Oder von Source
git clone https://github.com/ninja-build/ninja.git
cd ninja && ./configure.py --bootstrap
```

## Schnellstart

### 1. Projekt konfigurieren

```bash
# Standard-Konfiguration
./configure.py

# Mit benutzerdefinierten Optionen
./configure.py --aku-path /path/to/aku --opt-level 3

# Debug-Build
./configure.py --debug

# Release-Build
./configure.py --release
```

### 2. Bauen

```bash
# Alles bauen
ninja

# Nur LLVM IR generieren
ninja llvm

# Nur Assembly generieren
ninja asm

# Nur Präprozessor ausführen
ninja preprocess

# Executables bauen
ninja executables

# Spezifisches Target
ninja build/hello.o
ninja build/math.ll
```

### 3. Aufräumen

```bash
ninja clean
```

## Konfigurationsoptionen

### `configure.py` Optionen

```bash
./configure.py [options]

Options:
  --karelc PATH         Path to karelc compiler (default: karelc)
  --aku-path PATH       Path to AKU library (default: ../aku)
  --build-dir PATH      Build output directory (default: build)
  --opt-level LEVEL     Optimization level 0-3 (default: 2)
  --sources PATTERN     Source file pattern (default: test/*.kl)
  --debug               Enable debug mode (opt-level=0, keep preprocessor output)
  --release             Release mode (opt-level=3, strip symbols)
  --output FILE         Output ninja file (default: build.ninja)
  --help                Show help
```

### Beispiele

```bash
# Debug-Build mit Symbols
./configure.py --debug --build-dir debug

# Release-Build optimiert
./configure.py --release --build-dir release

# Andere Source-Dateien
./configure.py --sources "src/**/*.kl"

# Spezifischer Compiler-Pfad
./configure.py --karelc /usr/local/bin/karelc

# AKU-Bibliothek woanders
./configure.py --aku-path /opt/aku
```

## Build-Targets

Nach Konfiguration stehen folgende Targets zur Verfügung:

### Haupt-Targets

- **`all`** (default) - Kompiliert alle `.kl` Dateien zu `.o` Objektdateien
- **`llvm`** - Generiert LLVM IR (`.ll`) für alle Dateien
- **`asm`** - Generiert Assembly (`.s`) für alle Dateien
- **`preprocess`** - Führt nur Präprozessor aus, gibt `.pp.kl` aus
- **`executables`** - Linkt alle Programme zu ausführbaren Binaries
- **`clean`** - Entfernt Build-Verzeichnis

### Spezifische Targets

Für jede `.kl` Datei werden automatisch generiert:

```bash
# Für test/hello.kl:
build/hello.o         # Objektdatei
build/hello.ll        # LLVM IR
build/hello.s         # Assembly
build/hello.pp.kl     # Präprozessierte Datei
build/hello           # Executable
```

## Ninja-Befehle

### Informationen

```bash
# Alle verfügbaren Targets anzeigen
ninja -t targets

# Nur Haupt-Targets
ninja -t targets all

# Build-Graph anzeigen
ninja -t graph

# Abhängigkeiten eines Targets
ninja -t deps build/hello.o

# Warum Target gebaut wird
ninja -t explain build/hello.o
```

### Build-Optionen

```bash
# Verbose output
ninja -v

# Nur bestimmte Targets
ninja build/hello.o build/math.o

# Parallel builds (default: CPU cores)
ninja -j 8

# Dry-run (zeigt was gebaut würde)
ninja -n

# Build alles von vorne
ninja -t clean
ninja
```

## Projekt-Struktur

```
karel/
├── build.ninja.template   # Template für manuelle Anpassung
├── configure.py           # Konfigurations-Script
├── build.ninja           # Generierte Ninja-Datei (nach ./configure.py)
├── build/                # Build-Verzeichnis (erstellt von ninja)
│   ├── hello.o
│   ├── hello.ll
│   ├── hello.s
│   ├── hello.pp.kl
│   └── hello
└── test/                 # Source-Dateien
    ├── hello.kl
    ├── math.kl
    └── preprocessor.kl
```

## Workflow-Beispiele

### Standard-Entwicklung

```bash
# 1. Konfigurieren
./configure.py

# 2. Bauen
ninja

# 3. Bei Änderungen: Ninja baut automatisch nur geänderte Dateien
# Datei editieren: test/hello.kl
ninja  # Baut nur hello.kl neu

# 4. Debug-Modus
./configure.py --debug
ninja
```

### LLVM IR Debugging

```bash
# LLVM IR generieren
./configure.py
ninja llvm

# IR inspizieren
cat build/hello.ll
llvm-as build/hello.ll  # Validate
opt build/hello.ll -O3 -o build/hello.opt.ll  # Optimize
```

### Präprozessor-Debugging

```bash
# Nur Präprozessor ausführen
ninja preprocess

# Ergebnis anschauen
cat build/preprocessor.pp.kl

# Vergleichen mit Original
diff test/preprocessor.kl build/preprocessor.pp.kl
```

### Multi-Configuration Builds

```bash
# Debug-Build
./configure.py --debug --output debug.ninja
ninja -f debug.ninja

# Release-Build
./configure.py --release --output release.ninja
ninja -f release.ninja

# Beide parallel
ninja -f debug.ninja &
ninja -f release.ninja &
```

## Fortgeschrittene Verwendung

### Custom Build Rules

Editiere `build.ninja.template` und füge eigene Rules hinzu:

```ninja
rule karel_with_defines
  command = $karelc $in $karelflags -DDEBUG=1 -DVERSION=2 -o $out
  description = Compiling with defines: $in

build $build_dir/custom.o: karel_with_defines src/custom.kl
```

Dann: `./configure.py` (liest Template)

### Conditional Compilation

```ninja
# In build.ninja.template
rule karel_platform
  command = $karelc $in $karelflags -DPLATFORM=$platform -o $out
  description = Compiling for platform: $platform

build $build_dir/platform_x86.o: karel_platform src/platform.kl
  platform = PLATFORM_X86

build $build_dir/platform_arm.o: karel_platform src/platform.kl
  platform = PLATFORM_ARM
```

### Incremental Builds

Ninja verfolgt automatisch Abhängigkeiten:

```bash
# Erste Build
ninja                    # Baut alles

# Source-Datei ändern
touch test/hello.kl
ninja                    # Baut nur hello.kl neu

# Header ändern (wenn tracked)
touch aku/string/include/string_lib.h.kl
ninja                    # Baut alle abhängigen Dateien neu
```

## Performance

Ninja ist optimiert für Geschwindigkeit:

```bash
# Parallele Builds (nutzt alle CPU cores)
ninja -j $(nproc)

# Sehr große Projekte
ninja -j 16

# Mit Profiling
ninja -d stats
```

Benchmark-Vergleich (100 Karel-Dateien):

- **Make**: ~45s
- **Ninja**: ~8s
- **Ninja -j8**: ~2s

## Troubleshooting

### Ninja findet karelc nicht

```bash
# Pfad angeben
./configure.py --karelc /usr/local/bin/karelc

# Oder in PATH
export PATH=$PATH:/path/to/karel/bin
./configure.py
```

### Build-Fehler debuggen

```bash
# Verbose mode
ninja -v

# Einzelnes Target debuggen
ninja -v build/hello.o

# Ninja komplett neu starten
ninja -t clean
rm build.ninja
./configure.py
ninja
```

### Abhängigkeiten neu scannen

```bash
# build.ninja neu generieren
./configure.py

# Force rebuild
ninja -t clean
ninja
```

## Integration mit IDEs

### VSCode

`.vscode/tasks.json`:

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "Karel: Configure",
      "type": "shell",
      "command": "./configure.py",
      "group": "build"
    },
    {
      "label": "Karel: Build",
      "type": "shell",
      "command": "ninja",
      "group": {
        "kind": "build",
        "isDefault": true
      }
    },
    {
      "label": "Karel: Clean",
      "type": "shell",
      "command": "ninja clean"
    }
  ]
}
```

### CLion / IntelliJ

Settings → Build → Build Tools → Ninja:

- Ninja executable: `/usr/local/bin/ninja`
- Build directory: `build`
- Build file: `build.ninja`

### Vim/Neovim

```vim
" In .vimrc or init.vim
nnoremap <F5> :!./configure.py && ninja<CR>
nnoremap <F6> :!ninja<CR>
nnoremap <F7> :!ninja clean<CR>
```

## Best Practices

1. **Version Control**:

   - Commit: `build.ninja.template`, `configure.py`
   - Ignore: `build.ninja`, `build/`

2. **Clean Builds**:

   ```bash
   ninja clean && ./configure.py && ninja
   ```

3. **CI/CD Integration**:

   ```bash
   ./configure.py --release
   ninja -j $(nproc)
   ninja executables
   ```

4. **Development Workflow**:
   ```bash
   ./configure.py --debug    # Einmal
   # Dann nur noch:
   ninja                      # Nach Änderungen
   ```

## Siehe auch

- [Ninja Manual](https://ninja-build.org/manual.html)
- [Karel README](README.md)
- [Präprozessor Dokumentation](PREPROCESSOR.md)
