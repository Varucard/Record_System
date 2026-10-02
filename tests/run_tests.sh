#!/usr/bin/env bash
# Pruebas de punta a punta de Record System.
#
# Cada caso ejecuta el binario con una entrada simulada (stdin) sobre un
# directorio de datos temporal y verifica el texto de la salida.
#
# Uso: tests/run_tests.sh [ruta/al/binario]

set -u

BIN="${1:-./bin/record_system}"
if [[ ! -x "$BIN" ]]; then
    echo "No se encontró el binario '$BIN'. Ejecute 'make build'." >&2
    exit 2
fi
BIN="$(cd "$(dirname "$BIN")" && pwd)/$(basename "$BIN")"

WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

# Fecha fija para que las salidas sean reproducibles.
export RS_FECHA_HOY=20261002

PASSED=0
FAILED=0
CURRENT=""
OUTPUT=""
EXIT_CODE=0
DATA_DIR=""

# Inicia un caso con un directorio de datos nuevo.
start_case() {
    CURRENT="$1"
    DATA_DIR="$WORK_DIR/case_$((PASSED + FAILED + 1))"
    mkdir -p "$DATA_DIR"
}

# Ejecuta el sistema con las líneas recibidas como entrada.
run_system() {
    OUTPUT="$(printf '%s\n' "$@" | RS_DATA_DIR="$DATA_DIR" "$BIN" 2>&1)"
    EXIT_CODE=$?
}

expect_exit_code() {
    [[ -z "$CURRENT" ]] && return
    if [[ "$EXIT_CODE" -ne "$1" ]]; then
        fail "código de salida esperado $1, obtenido $EXIT_CODE"
    fi
}

fail() {
    echo "  FALLA: $CURRENT"
    echo "         $1"
    FAILED=$((FAILED + 1))
    CURRENT=""
}

expect() {
    [[ -z "$CURRENT" ]] && return
    if ! grep -qF -- "$1" <<<"$OUTPUT"; then
        fail "se esperaba encontrar: '$1'"
    fi
}

expect_not() {
    [[ -z "$CURRENT" ]] && return
    if grep -qF -- "$1" <<<"$OUTPUT"; then
        fail "no se esperaba encontrar: '$1'"
    fi
}

expect_count() {
    [[ -z "$CURRENT" ]] && return
    local count
    count="$(grep -cF -- "$2" <<<"$OUTPUT")"
    if [[ "$count" -ne "$1" ]]; then
        fail "se esperaban $1 apariciones de '$2' y hubo $count"
    fi
}

end_case() {
    if [[ -n "$CURRENT" ]]; then
        echo "  ok    $CURRENT"
        PASSED=$((PASSED + 1))
    else
        echo "--- salida del caso fallido ---"
        echo "$OUTPUT" | tail -n 40
        echo "-------------------------------"
    fi
}

# Secuencias reutilizables de entrada -----------------------------------
# Alta de cliente desde el menú principal (vuelve al menú principal).
alta_cliente() { # dni nombre
    printf '%s\n' 1 1 "$1" "$2" 1155550000 "cliente@mail.com" "Calle 123" 0
}
# Alta de equipo desde el menú principal.
alta_equipo() { # dni tipo descripcion
    printf '%s\n' 2 1 "$1" "$2" "$3" "" "No enciende" 0
}
# Alta de presupuesto desde el menú principal.
alta_presupuesto() { # equipo descripcion importe
    printf '%s\n' 3 1 "$1" "$2" "$3" "" 0
}

# Carga en _LINES las líneas de la entrada estándar.
lines() { mapfile -t _LINES; }

echo "Ejecutando pruebas de Record System"

# -----------------------------------------------------------------------
start_case "al iniciar crea los archivos de datos y sale con 0"
run_system 0
expect "[CREADO] customers.dat"
expect "[CREADO] equipments.dat"
expect "[CREADO] budgets.dat"
expect "Hasta luego."
expect_exit_code 0
[[ -f "$DATA_DIR/customers.dat" ]] || fail "no se creó customers.dat"
end_case

# -----------------------------------------------------------------------
start_case "los datos persisten entre ejecuciones"
lines < <(alta_cliente 30123456 "Juan Pérez"; echo 0)
run_system "${_LINES[@]}"
expect "Cliente registrado correctamente."
run_system 1 3 0 0
expect "[OK]     customers.dat"
expect "30123456 Juan Pérez"
expect "Total de clientes: 1"
end_case

# -----------------------------------------------------------------------
start_case "valida DNI, cliente duplicado y email"
lines < <(alta_cliente 30123456 "Juan Pérez"; echo 0)
run_system "${_LINES[@]}"
run_system 1 1 12AB 123 30123456 "" 1 \
    4111222 "Ana Gómez" "" "sin-arroba" "ana@mail" "ana@mail.com" "" 0 0
expect "DNI inválido: debe tener 7 u 8 dígitos."
expect "Ya existe un cliente con DNI 30123456"
expect_count 2 "Email inválido"
expect "Cliente registrado correctamente."
end_case

# -----------------------------------------------------------------------
start_case "modificar cliente mantiene los valores con ENTER"
lines < <(alta_cliente 30123456 "Juan Pérez"; echo 0)
run_system "${_LINES[@]}"
run_system 1 4 30123456 "" 1199998888 "" "" 2 30123456 0 0
expect "Cliente actualizado correctamente."
expect "Nombre:     Juan Pérez"
expect "Teléfono:   1199998888"
expect "Email:      cliente@mail.com"
end_case

# -----------------------------------------------------------------------
start_case "no permite equipos de clientes inexistentes"
run_system 2 1 99999999 0 0
expect "No existe un cliente con DNI 99999999"
end_case

# -----------------------------------------------------------------------
start_case "numera equipos y los lista por cliente"
lines < <(alta_cliente 30123456 "Juan Pérez"
          alta_cliente 4111222 "Ana Gómez"
          alta_equipo 30123456 Notebook "Lenovo T14"
          alta_equipo 4111222 PC "Gamer Ryzen"
          alta_equipo 30123456 Impresora "HP 1102"
          echo 0)
run_system "${_LINES[@]}"
expect "Equipo registrado con el número 1."
expect "Equipo registrado con el número 2."
expect "Equipo registrado con el número 3."
run_system 2 4 30123456 0 0
expect "Total de equipos: 2"
expect "00003 30123456 Impresora"
expect_not "Gamer Ryzen"
end_case

# -----------------------------------------------------------------------
start_case "cambia el estado de un equipo"
lines < <(alta_cliente 30123456 "Juan Pérez"
          alta_equipo 30123456 Notebook "Lenovo T14"
          echo 0)
run_system "${_LINES[@]}"
run_system 2 6 1 9 3 2 1 0 0
expect "Opción inválida."
expect "Equipo actualizado correctamente."
expect "Estado:          Listo p/ retirar"
end_case

# -----------------------------------------------------------------------
start_case "crea presupuestos validando importe y fecha"
lines < <(alta_cliente 30123456 "Juan Pérez"
          alta_equipo 30123456 Notebook "Lenovo T14"
          echo 0)
run_system "${_LINES[@]}"
run_system 3 1 77 1 1 "Cambio de pantalla" "abc" "0" "-500" "85000,75" \
    "31/02/2026" "2026-03-01" "01/03/2026" 2 1 0 0
expect "No existe el equipo número 77."
expect_count 3 "Importe inválido"
expect_count 2 "Fecha inválida"
expect "Presupuesto registrado con el número 1."
expect "Importe:         \$ 85.000,75"
expect "Fecha:           01/03/2026"
expect "PENDIENTE DE PAGO"
end_case

# -----------------------------------------------------------------------
start_case "registra pagos y protege presupuestos pagados"
lines < <(alta_cliente 30123456 "Juan Pérez"
          alta_equipo 30123456 Notebook "Lenovo T14"
          alta_presupuesto 1 "Cambio de fuente" 45000
          alta_presupuesto 1 "Limpieza" 12000.50
          echo 0)
run_system "${_LINES[@]}"
run_system 3 6 1 5 "" \
           6 1 \
           7 1 \
           8 1 \
           5 0 0
expect "Pago registrado correctamente."
expect "PAGADO el 02/10/2026 (Mercado Pago)"
expect "El presupuesto ya está pagado."
expect "No se puede modificar un presupuesto pagado."
expect "No se puede eliminar un presupuesto pagado."
expect "Total: 1 presupuesto(s) por \$ 12.000,50"
end_case

# -----------------------------------------------------------------------
start_case "respeta la integridad al eliminar"
lines < <(alta_cliente 30123456 "Juan Pérez"
          alta_equipo 30123456 Notebook "Lenovo T14"
          alta_presupuesto 1 "Cambio de fuente" 45000
          echo 0)
run_system "${_LINES[@]}"
run_system 1 5 30123456 0 \
           2 7 1 0 \
           3 8 1 x n 8 1 s 0 \
           2 7 1 S 0 \
           1 5 30123456 S 3 0 0
expect "el cliente tiene 1 equipo(s) registrado(s)."
expect "el equipo tiene 1 presupuesto(s)."
expect "Responda S o N."
expect "Operación cancelada."
expect "Presupuesto eliminado."
expect "Equipo eliminado."
expect "Cliente eliminado."
expect "Total de clientes: 0"
end_case

# -----------------------------------------------------------------------
start_case "el reporte resume equipos, cobros y deuda"
lines < <(alta_cliente 30123456 "Juan Pérez"
          alta_equipo 30123456 Notebook "Lenovo T14"
          alta_equipo 30123456 PC "Gamer"
          alta_presupuesto 1 "Cambio de fuente" 45000
          alta_presupuesto 2 "Limpieza" 12000.50
          alta_presupuesto 2 "Pasta térmica" 3000
          printf '%s\n' 3 6 1 1 "" 6 3 1 "" 0
          printf '%s\n' 2 6 2 2 0
          echo 0)
run_system "${_LINES[@]}"
run_system 4 1 0 0
expect "Clientes registrados:               1"
expect "Equipos registrados:                2"
expect "Presupuestos emitidos:              3"
expect "Total cobrado:              \$         48.000,00"
expect "Total pendiente de cobro:   \$         12.000,50"
expect "- Efectivo           2 pago(s)  \$         48.000,00"
end_case

# -----------------------------------------------------------------------
start_case "cierra ordenadamente si la entrada termina"
run_system 1 1 30123456
expect "Fin de la entrada. Cerrando el sistema."
expect_not "implicit CLOSE"
expect_exit_code 2
end_case

# -----------------------------------------------------------------------
start_case "normaliza el DNI: 1234567 y 01234567 son el mismo cliente"
lines < <(alta_cliente 1234567 "Ana Gómez"; alta_cliente 01234567 "Otra"
          echo 0)
run_system "${_LINES[@]}"
expect "Ya existe un cliente con DNI 01234567: Ana Gómez"
expect_count 1 "Cliente registrado correctamente."
end_case

# -----------------------------------------------------------------------
start_case "interpreta importes en formato argentino"
lines < <(alta_cliente 30123456 "Juan Pérez"
          alta_equipo 30123456 Notebook "Lenovo T14"
          alta_presupuesto 1 "Uno" "15.000"
          alta_presupuesto 1 "Dos" "1.500,50"
          alta_presupuesto 1 "Tres" "12.5"
          alta_presupuesto 1 "Cuatro" "2000000"
          echo 0)
run_system "${_LINES[@]}"
run_system 3 3 0 0
expect "15.000,00"
expect "1.500,50"
expect "12,50"
expect "2.000.000,00"
expect "Total: 4 presupuesto(s) por \$ 2.016.513,00"
end_case

# -----------------------------------------------------------------------
start_case "acepta entradas con finales de línea de Windows (CRLF)"
run_system $'1\r' $'1\r' $'30123456\r' $'Juan\r' $'\r' $'\r' $'\r' \
           $'0\r' $'0\r'
expect "Cliente registrado correctamente."
expect "Hasta luego."
expect_not "Opción inválida."
end_case

# -----------------------------------------------------------------------
start_case "rechaza textos que no entran en el campo"
run_system 1 1 30123456 "$(printf 'N%.0s' {1..41})" "Juan" \
           "" "$(printf 'a%.0s' {1..45})@mail.com" "" "" 0 0
expect "Texto demasiado largo: máximo 40 caracteres."
expect "Texto demasiado largo: máximo 50 caracteres."
expect "Cliente registrado correctamente."
end_case

# -----------------------------------------------------------------------
start_case "modificar con - borra un dato opcional"
lines < <(alta_cliente 30123456 "Juan Pérez"; echo 0)
run_system "${_LINES[@]}"
run_system 1 4 30123456 "" "-" "-" "" 0 0
expect "Cliente actualizado correctamente."
run_system 1 2 30123456 0 0
expect_not "1155550000"
expect_not "cliente@mail.com"
expect "Dirección:  Calle 123"
end_case

# -----------------------------------------------------------------------
start_case "pagina los listados y permite cortarlos"
lines < <(for i in $(seq 10000001 10000021); do
              alta_cliente "$i" "Cliente $i"
          done
          echo 0)
run_system "${_LINES[@]}"
run_system 1 3 "" 0 0
expect_count 1 "ENTER para continuar"
expect "Total de clientes: 21"
run_system 1 3 0 0 0
expect "Total de clientes: 20"
expect_not "10000021 Cliente"
end_case

# -----------------------------------------------------------------------
start_case "no pausa un listado de exactamente una página"
lines < <(for i in $(seq 10000001 10000020); do
              alta_cliente "$i" "Cliente $i"
          done
          echo 0)
run_system "${_LINES[@]}"
run_system 1 3 0 0
expect_not "ENTER para continuar"
expect "Total de clientes: 20"
expect "Hasta luego."
end_case

# -----------------------------------------------------------------------
start_case "la confirmación sólo acepta S/SI/N/NO"
lines < <(alta_cliente 30123456 "Juan Pérez"; echo 0)
run_system "${_LINES[@]}"
run_system 1 5 30123456 sarasa no 0 0
expect "Responda S o N."
expect "Operación cancelada."
expect_not "Cliente eliminado."
end_case

# -----------------------------------------------------------------------
start_case "avisa al entregar un equipo con deuda y valida fecha de pago"
lines < <(alta_cliente 30123456 "Juan Pérez"
          alta_equipo 30123456 Notebook "Lenovo T14"
          printf '%s\n' 3 1 1 "Fuente" 45000 "10/09/2026" 0
          echo 0)
run_system "${_LINES[@]}"
run_system 2 6 1 4 N 0 \
           3 6 1 1 "01/09/2026" "15/09/2026" 0 \
           2 6 1 4 0 0
expect "el equipo tiene 1 presupuesto(s) pendiente(s) de pago."
expect "Operación cancelada."
expect "La fecha no puede ser anterior al 10/09/2026."
expect "Pago registrado correctamente."
expect_count 1 "Equipo actualizado correctamente."
end_case

# -----------------------------------------------------------------------
start_case "no reutiliza números de equipos ni presupuestos borrados"
lines < <(alta_cliente 30123456 "Juan Pérez"
          alta_equipo 30123456 Notebook "Uno"
          alta_equipo 30123456 Notebook "Dos"
          alta_presupuesto 1 "Primero" 1000
          alta_presupuesto 1 "Segundo" 2000
          printf '%s\n' 2 7 2 S 0
          printf '%s\n' 3 8 2 S 0
          alta_equipo 30123456 PC "Tres"
          alta_presupuesto 1 "Tercero" 3000
          echo 0)
run_system "${_LINES[@]}"
expect "Equipo eliminado."
expect "Presupuesto eliminado."
expect "Equipo registrado con el número 3."
expect "Presupuesto registrado con el número 3."
end_case

# -----------------------------------------------------------------------
start_case "busca clientes por parte del nombre"
lines < <(alta_cliente 30123456 "Juan Pérez"
          alta_cliente 4111222 "Ana Gómez"
          alta_cliente 22333444 "JUANA Ruiz"
          echo 0)
run_system "${_LINES[@]}"
run_system 1 6 juan 0 0
expect "Clientes encontrados: 2"
expect "30123456 Juan Pérez"
expect "22333444 JUANA Ruiz"
expect_not "Ana Gómez"
end_case

# -----------------------------------------------------------------------
start_case "muestra el estado de cuenta de un cliente"
lines < <(alta_cliente 30123456 "Juan Pérez"
          alta_cliente 4111222 "Ana Gómez"
          alta_equipo 30123456 Notebook "Lenovo"
          alta_equipo 4111222 PC "Gamer"
          alta_presupuesto 1 "Fuente" 45000
          alta_presupuesto 1 "Limpieza" "12.000,50"
          alta_presupuesto 2 "Otro cliente" 99000
          printf '%s\n' 3 6 1 1 "" 0
          echo 0)
run_system "${_LINES[@]}"
run_system 3 9 30123456 0 0
expect "Estado de cuenta: Juan Pérez (DNI 30123456)"
expect "Total: 2 presupuesto(s) por \$ 57.000,50"
expect "Saldo adeudado: \$ 12.000,50"
expect_not "Otro cliente"
end_case

# -----------------------------------------------------------------------
start_case "genera comprobantes de ingreso, presupuesto y recibo"
lines < <(alta_cliente 30123456 "Juan Pérez"
          alta_equipo 30123456 Notebook "Lenovo T14"
          alta_presupuesto 1 "Cambio de fuente" 45000
          alta_presupuesto 1 "Limpieza" 5000
          printf '%s\n' 3 6 2 4 "" 0
          echo 0)
run_system "${_LINES[@]}"
OUTPUT="$(printf '%s\n' 2 8 1 0 3 10 1 10 2 0 0 \
    | RS_EMPRESA="Taller Varucard" RS_DATA_DIR="$DATA_DIR" "$BIN" 2>&1)"
expect "comprobantes/ingreso-00001.txt"
expect "comprobantes/presupuesto-00001.txt"
expect "comprobantes/recibo-00002.txt"
OUTPUT="$(cat "$DATA_DIR/comprobantes/ingreso-00001.txt" 2>&1)"
expect "Taller Varucard"
expect "COMPROBANTE DE INGRESO DE EQUIPO N. 1"
expect "Nombre:          Juan Pérez"
expect "Descripción:     Lenovo T14"
expect "Firma del cliente"
OUTPUT="$(cat "$DATA_DIR/comprobantes/presupuesto-00001.txt" 2>&1)"
expect "PRESUPUESTO N. 1"
expect "Importe:         \$ 45.000,00"
expect "PENDIENTE DE PAGO"
OUTPUT="$(cat "$DATA_DIR/comprobantes/recibo-00002.txt" 2>&1)"
expect "RECIBO DE PAGO - PRESUPUESTO N. 2"
expect "Forma de pago:   Transferencia"
end_case

# -----------------------------------------------------------------------
start_case "exporta los datos a CSV para Excel"
lines < <(alta_cliente 30123456 "Juan Pérez"
          alta_equipo 30123456 Notebook 'Pantalla "rota"; urgente'
          alta_presupuesto 1 "Cambio de fuente" "45.000,50"
          echo 0)
run_system "${_LINES[@]}"
run_system 4 2 0 0
expect "exportes/clientes.csv"
expect "(1 registro(s))"
CSV_DIR="$DATA_DIR/exportes"
OUTPUT="$(head -c 3 "$CSV_DIR/clientes.csv" | od -An -tx1 | tr -d ' ')"
expect "efbbbf"
OUTPUT="$(cat "$CSV_DIR/clientes.csv" "$CSV_DIR/equipos.csv" \
              "$CSV_DIR/presupuestos.csv" 2>&1)"
expect '"DNI";"Nombre";"Teléfono";"Email";"Dirección";"Fecha de alta"'
expect '"30123456";"Juan Pérez";"1155550000";"cliente@mail.com";"Calle 123";"02/10/2026"'
expect '"Pantalla ""rota""; urgente"'
expect ';45000,50;'
expect '"No";""'
end_case

# -----------------------------------------------------------------------
start_case "un * cancela el alta en cualquier dato"
lines < <(alta_cliente 30123456 "Juan Pérez"
          alta_equipo 30123456 Notebook "Lenovo"
          echo 0)
run_system "${_LINES[@]}"
run_system 1 1 22333444 "Ana" "*" 3 0 \
           3 1 1 "Trabajo" "*" 3 0 0
expect "(Escriba * en cualquier dato para cancelar.)"
expect_count 2 "Operación cancelada."
expect "Total de clientes: 1"
expect "Total: 0 presupuesto(s)"
expect_not "Email (opcional):"
end_case

# -----------------------------------------------------------------------
start_case "recorta textos largos sin partir caracteres acentuados"
lines < <(alta_cliente 30123456 "Juan Pérez"
          alta_equipo 30123456 "Notebook" \
              "$(printf 'á%.0s' {1..40})"
          echo 0)
run_system "${_LINES[@]}"
run_system 2 3 0 0
expect "Notebook        $(printf 'á%.0s' {1..30}) Ingresado"
if ! iconv -f UTF-8 -t UTF-8 <<<"$OUTPUT" >/dev/null 2>&1; then
    fail "la salida contiene UTF-8 inválido"
fi
end_case

# -----------------------------------------------------------------------
start_case "la interfaz clásica no usa secuencias de escape"
run_system 1 0 0
expect_not $'\e['
end_case

# -----------------------------------------------------------------------
start_case "el modo pantalla limpia la pantalla y pausa tras cada acción"
OUTPUT="$(printf '%s\n' "" 1 3 "" 0 0 \
    | RS_INTERFAZ=pantalla RS_TEMA=verde RS_DATA_DIR="$DATA_DIR" \
      "$BIN" 2>&1)"
EXIT_CODE=$?
expect $'\e[2J'
expect $'\e[0;32m'
expect "| "
expect "Presione ENTER para continuar..."
expect "Hasta luego."
expect_exit_code 0
end_case

# -----------------------------------------------------------------------
if command -v flock >/dev/null 2>&1; then
    start_case "el lanzador impide abrir dos instancias sobre los mismos datos"
    LAUNCHER="$(cd "$(dirname "$0")/.." && pwd)/scripts/record-system.sh"
    ( sleep 3 | RS_BIN="$BIN" RS_DATA_DIR="$DATA_DIR" "$LAUNCHER" \
        >/dev/null 2>&1 ) &
    sleep 1
    OUTPUT="$(echo 0 | RS_BIN="$BIN" RS_DATA_DIR="$DATA_DIR" "$LAUNCHER" 2>&1)"
    EXIT_CODE=$?
    wait
    expect "ya está abierto"
    expect_exit_code 75
    OUTPUT="$(echo 0 | RS_BIN="$BIN" RS_DATA_DIR="$DATA_DIR/nuevo/dir" \
        "$LAUNCHER" 2>&1)"
    EXIT_CODE=$?
    expect "[CREADO] customers.dat"
    expect_exit_code 0
    end_case
fi

# -----------------------------------------------------------------------
echo
echo "Resultado: $PASSED ok, $FAILED con fallas."
[[ "$FAILED" -eq 0 ]]
