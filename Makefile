# Record System - tareas de compilación, ejecución y pruebas.
# Requiere GnuCOBOL 3.x (cobc). Sin GnuCOBOL local, usar los
# objetivos docker-* (ver README).

COBC      ?= cobc
COBFLAGS  ?= -std=default -Wall -fstatic-call -I src/copybooks
BIN_DIR   := bin
TARGET    := $(BIN_DIR)/record_system
DATA_DIR  ?= data

# El programa principal va primero: es el punto de entrada (-x).
SOURCES   := src/record_system.cbl \
             src/verificar_archivos.cbl \
             src/clientes.cbl \
             src/equipos.cbl \
             src/presupuestos.cbl \
             src/reportes.cbl
COPYBOOKS := $(wildcard src/copybooks/*.cpy)

EXAMPLES_DIR := examples/tutorial
EXAMPLES     := $(patsubst $(EXAMPLES_DIR)/%.cbl,$(BIN_DIR)/examples/%,\
                $(wildcard $(EXAMPLES_DIR)/*.cbl))

DOCKER_IMAGE := record-system

.PHONY: all build run test lint examples backup clean \
        docker-build docker-run docker-test help

all: build

help:
	@echo "Objetivos disponibles:"
	@echo "  make build         Compila el sistema en $(TARGET)"
	@echo "  make run           Compila y ejecuta (datos en ./$(DATA_DIR))"
	@echo "  make backup        Copia los datos a backups/<fecha-hora>/"
	@echo "  make test          Ejecuta las pruebas automatizadas"
	@echo "  make lint          Verifica el formato fijo (columna 72)"
	@echo "  make examples      Compila los ejemplos del tutorial"
	@echo "  make clean         Borra los binarios generados"
	@echo "  make docker-build  Construye la imagen Docker"
	@echo "  make docker-run    Ejecuta el sistema dentro de Docker"
	@echo "  make docker-test   Ejecuta las pruebas dentro de Docker"

build: $(TARGET)

$(TARGET): $(SOURCES) $(COPYBOOKS)
	@mkdir -p $(BIN_DIR)
	$(COBC) -x $(COBFLAGS) -o $@ $(SOURCES)

run: build
	RS_DATA_DIR=$(DATA_DIR) RS_BIN=$(abspath $(TARGET)) \
		./scripts/record-system.sh

test: build lint
	./tests/run_tests.sh $(TARGET)

lint:
	@./tests/check_format.sh src examples/tutorial

examples: $(EXAMPLES)

$(BIN_DIR)/examples/%: $(EXAMPLES_DIR)/%.cbl
	@mkdir -p $(BIN_DIR)/examples
	$(COBC) -x -std=default -I $(EXAMPLES_DIR) -o $@ $<

# Copia los .dat junto con sus índices (.dat.1, .dat.2...), que no
# sirven por separado.
backup:
	@test -d $(DATA_DIR) || { echo "No existe $(DATA_DIR)/"; exit 1; }
	@dest=backups/$$(date +%Y%m%d-%H%M%S); mkdir -p $$dest && \
		cp -p $(DATA_DIR)/*.dat* $$dest/ && echo "Respaldo en $$dest/"

clean:
	rm -rf $(BIN_DIR)

docker-build:
	docker build -t $(DOCKER_IMAGE) .

docker-run: docker-build
	docker compose run --rm record-system

docker-test:
	docker build --target test -t $(DOCKER_IMAGE):test .
