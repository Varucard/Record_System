#!/bin/sh
# Lanzador de Record System.
#
#  - Crea el directorio de datos (incluidos los intermedios).
#  - Verifica que se pueda escribir en él.
#  - Impide abrir dos instancias sobre los mismos datos: los archivos
#    indexados no admiten escrituras concurrentes y se perderían datos.
#
# Opciones:
#   --pantalla   interfaz de pantalla completa (RS_INTERFAZ=pantalla)
#   --verde      tema de fósforo verde para el modo pantalla
#   --clasica    interfaz clásica de línea (por defecto)
#
# Variables: RS_DATA_DIR (por defecto "data") y RS_BIN (ruta del
# ejecutable; por defecto "record_system" del PATH).

for opcion in "$@"; do
    case "$opcion" in
        --pantalla) export RS_INTERFAZ=pantalla ;;
        --verde)    export RS_INTERFAZ=pantalla RS_TEMA=verde ;;
        --clasica)  export RS_INTERFAZ=clasica ;;
        -h|--help)
            echo "Uso: record-system [--clasica | --pantalla | --verde]"
            exit 0 ;;
        *)
            echo "Opción desconocida: $opcion (use --help)" >&2
            exit 64 ;;
    esac
done

DATA_DIR="${RS_DATA_DIR:-data}"
BIN="${RS_BIN:-record_system}"
LOCK_FILE="$DATA_DIR/.record_system.lock"

if ! mkdir -p "$DATA_DIR" 2>/dev/null || [ ! -w "$DATA_DIR" ]; then
    echo "Error: no se puede escribir en el directorio de datos '$DATA_DIR'" >&2
    echo "(usuario $(id -u):$(id -g)). Revise los permisos de la carpeta." >&2
    exit 1
fi

export RS_DATA_DIR="$DATA_DIR"

if command -v flock >/dev/null 2>&1; then
    exec 9>"$LOCK_FILE"
    if ! flock -n 9; then
        echo "Error: Record System ya está abierto sobre '$DATA_DIR'." >&2
        echo "Cierre la otra instancia antes de continuar." >&2
        exit 75
    fi
else
    echo "Aviso: 'flock' no está disponible; no se controla que haya" >&2
    echo "una sola instancia abierta." >&2
fi

"$BIN"
