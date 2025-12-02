# Karel Preprocessor

Der Karel-Compiler enthält einen vollständigen C-ähnlichen Präprozessor mit `%` als Präfix (analog zu `#` in C).

## Unterstützte Direktiven

### Makro-Definition

#### `%DEFINE name value`

Definiert eine Konstante oder ein Makro:

```karel
%DEFINE DEBUG 1
%DEFINE MAX_SIZE 100
%DEFINE PI 3.14159
%DEFINE VERSION 2
```

Makros können in Ausdrücken verwendet werden:

```karel
VAR
  size : INTEGER

size = MAX_SIZE  -- Wird zu: size = 100
```

#### `%UNDEF name`

Entfernt eine Makro-Definition:

```karel
%DEFINE TEMP 42
%UNDEF TEMP  -- TEMP ist jetzt nicht mehr definiert
```

### Bedingte Kompilierung

#### `%IFDEF name`

Code wird nur kompiliert, wenn das Makro definiert ist:

```karel
%DEFINE DEBUG 1

%IFDEF DEBUG
  WRITE('Debug mode enabled')  -- Wird kompiliert
%ENDIF
```

#### `%IFNDEF name`

Code wird nur kompiliert, wenn das Makro **nicht** definiert ist:

```karel
%IFNDEF RELEASE
  WRITE('Development build')  -- Wird kompiliert wenn RELEASE nicht definiert
%ENDIF
```

#### `%IF expression`

Code wird basierend auf einem Ausdruck kompiliert:

```karel
%DEFINE VERSION 2

%IF VERSION >= 2
  WRITE('Using new API')  -- Wird kompiliert
%ELSE
  WRITE('Using old API')  -- Wird übersprungen
%ENDIF
```

Unterstützte Operatoren in `%IF`:

- Vergleich: `=`, `<>`, `<`, `>`, `<=`, `>=`
- Logik: `AND`, `OR`, `NOT`
- Arithmetik: `+`, `-`, `*`, `/`, `MOD`

#### `%ELSE`

Alternative für `%IF`, `%IFDEF`, `%IFNDEF`:

```karel
%IFDEF DEBUG
  -- Debug code
%ELSE
  -- Release code
%ENDIF
```

#### `%ENDIF`

Beendet einen bedingten Block:

```karel
%IF VERSION = 1
  -- Code für Version 1
%ENDIF
```

### Include-Direktive

#### `%INCLUDE "file"`

Fügt eine andere Karel-Datei ein:

```karel
%INCLUDE "string_lib.h.kl"
%INCLUDE "math_utils.kl"
```

Include-Dateien werden in folgenden Pfaden gesucht:

- AKU-Projektverzeichnis (mit `--aku-path` angegeben)
- Unterverzeichnisse: `string/`, `math/`, `byte/`, etc.
- Include-Unterverzeichnisse: `string/include/`, `math/include/`, etc.

## Beispiele

### Beispiel 1: Debug-Modus

```karel
%DEFINE DEBUG 1

PROGRAM example
BEGIN
  %IFDEF DEBUG
    WRITE('Starting program in debug mode')
  %ENDIF

  -- Hauptcode
  WRITE('Hello')

  %IFDEF DEBUG
    WRITE('Program finished')
  %ENDIF
END example
```

### Beispiel 2: Versions-basierte Kompilierung

```karel
%DEFINE API_VERSION 2

PROGRAM api_example
BEGIN
  %IF API_VERSION >= 2
    -- Neue API verwenden
    WRITE('Using API v2')
  %ELSE
    -- Alte API verwenden
    WRITE('Using API v1')
  %ENDIF
END api_example
```

### Beispiel 3: Plattform-spezifischer Code

```karel
%DEFINE PLATFORM_FANUC 1
%DEFINE PLATFORM_SIM 2

%DEFINE TARGET_PLATFORM PLATFORM_FANUC

PROGRAM platform_demo
BEGIN
  %IF TARGET_PLATFORM = PLATFORM_FANUC
    -- FANUC-spezifischer Code
    %INCLUDE "fanuc_runtime.h.kl"
    WRITE('Running on FANUC robot')
  %ENDIF

  %IF TARGET_PLATFORM = PLATFORM_SIM
    -- Simulator-Code
    WRITE('Running in simulator')
  %ENDIF
END platform_demo
```

### Beispiel 4: Konfigurations-Makros

```karel
%DEFINE ROBOT_MODEL 3
%DEFINE HAS_VISION TRUE
%DEFINE MAX_SPEED 250

%IF ROBOT_MODEL = 1
  %DEFINE AXES 6
  %DEFINE PAYLOAD 50
%ENDIF

%IF ROBOT_MODEL = 3
  %DEFINE AXES 7
  %DEFINE PAYLOAD 150
%ENDIF

PROGRAM config
BEGIN
  VAR
    speed : INTEGER

  speed = MAX_SPEED  -- Wird zu: speed = 250

  WRITE('Robot has ')
  WRITE(AXES)  -- Wird zu: WRITE(7)
  WRITE(' axes')

  %IFDEF HAS_VISION
    WRITE('Vision system enabled')
  %ENDIF
END config
```

## Verwendung

### Mit Präprozessor kompilieren (Standard)

```bash
karelc program.kl -o program.o
```

### Nur Präprozessor ausführen

```bash
karelc program.kl --precompile
```

Dies gibt den vorverarbeiteten Code aus, ohne LLVM-Codegenerierung.

### Präprozessor überspringen

```bash
karelc program.kl --no-precompile -o program.o
```

### Mit benutzerdefiniertem AKU-Pfad

```bash
karelc program.kl --aku-path /path/to/aku -o program.o
```

## Verschachtelung

Bedingte Blöcke können verschachtelt werden:

```karel
%DEFINE DEBUG 1
%DEFINE VERBOSE 1

%IFDEF DEBUG
  WRITE('Debug enabled')

  %IFDEF VERBOSE
    WRITE('Verbose logging enabled')
  %ENDIF
%ENDIF
```

## Makro-Expansion

Makros werden in folgenden Kontexten expandiert:

1. **Konstanten-Deklarationen:**

```karel
%DEFINE MAX 100
CONST
  limit : INTEGER = MAX  -- Wird zu: limit = 100
```

2. **Variablen-Initialisierung:**

```karel
%DEFINE DEFAULT_SPEED 50
VAR
  speed : INTEGER = DEFAULT_SPEED  -- Wird zu: speed = 50
```

3. **Ausdrücke:**

```karel
%DEFINE FACTOR 2
size = value * FACTOR  -- Wird zu: size = value * 2
```

4. **Bedingungen:**

```karel
%DEFINE THRESHOLD 100
IF x > THRESHOLD THEN  -- Wird zu: IF x > 100 THEN
  -- Code
ENDIF
```

## Unterschiede zu C-Präprozessor

| C          | Karel      | Bemerkung                              |
| ---------- | ---------- | -------------------------------------- |
| `#define`  | `%DEFINE`  | Makro-Definition                       |
| `#undef`   | `%UNDEF`   | Makro entfernen                        |
| `#ifdef`   | `%IFDEF`   | Bedingte Kompilierung                  |
| `#ifndef`  | `%IFNDEF`  | Negierte Bedingung                     |
| `#if`      | `%IF`      | Ausdrucks-Bedingung                    |
| `#else`    | `%ELSE`    | Alternative                            |
| `#endif`   | `%ENDIF`   | Block-Ende                             |
| `#include` | `%INCLUDE` | Datei einbinden                        |
| `#pragma`  | `%PRAGMA`  | Compiler-Direktiven (FANUC-spezifisch) |

## Fehlerbehandlung

Der Präprozessor gibt Warnungen aus bei:

- Nicht gefundenen Include-Dateien
- `%ELSE` ohne vorheriges `%IF`
- `%ENDIF` ohne vorheriges `%IF`
- Undefinierte Makros in `%IF`-Ausdrücken (werden als 0/FALSE behandelt)

Beispiel:

```
Precompiler: Warning - Include not found: missing_file.kl
Precompiler: Warning - %ELSE without %IF
```

## Best Practices

1. **Makronamen in GROSSBUCHSTABEN:**

```karel
%DEFINE MAX_SIZE 100  -- Gut
%DEFINE max_size 100  -- Vermeiden
```

2. **Guards für Include-Dateien:**

```karel
-- In header.h.kl:
%IFNDEF HEADER_H_KL
%DEFINE HEADER_H_KL

-- Deklarationen hier

%ENDIF
```

3. **Dokumentation von Makros:**

```karel
-- Debug level: 0=off, 1=errors, 2=warnings, 3=verbose
%DEFINE DEBUG_LEVEL 2
```

4. **Plattform-Makros zentral definieren:**

```karel
-- config.kl
%DEFINE PLATFORM FANUC
%DEFINE VERSION 2
%INCLUDE "config.kl"
```

## Implementation

Der Präprozessor ist in `src/Precompiler.hs` implementiert und arbeitet in folgenden Schritten:

1. **Direktiven verarbeiten:** Parse `%DEFINE`, `%IF`, etc.
2. **Makro-Tabelle aufbauen:** Speichere definierte Makros
3. **Bedingungen evaluieren:** Werte `%IF`-Ausdrücke aus
4. **Code filtern:** Entferne inaktive Blöcke
5. **Makros expandieren:** Ersetze Makro-Namen durch Werte
6. **Includes auflösen:** Lade und parse Include-Dateien

Der Präprozessor wird automatisch vor der LLVM-Codegenerierung ausgeführt, kann aber auch standalone verwendet werden.
