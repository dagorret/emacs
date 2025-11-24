#!/usr/bin/env bash
set -e

echo "Instalando configuración de Emacs en $HOME ..."

BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ -d "$BASE_DIR/.emacs.d" ]; then
  echo "Copiando .emacs.d a $HOME/.emacs.d ..."
  rm -rf "$HOME/.emacs.d.bak"
  if [ -d "$HOME/.emacs.d" ]; then
    mv "$HOME/.emacs.d" "$HOME/.emacs.d.bak"
  fi
  cp -r "$BASE_DIR/.emacs.d" "$HOME/.emacs.d"
fi

if [ -d "$BASE_DIR/org" ]; then
  echo "Copiando org/ a $HOME/org ..."
  rm -rf "$HOME/org.bak"
  if [ -d "$HOME/org" ]; then
    mv "$HOME/org" "$HOME/org.bak"
  fi
  cp -r "$BASE_DIR/org" "$HOME/org"
fi

if [ -d "$BASE_DIR/notas" ]; then
  echo "Copiando notas/ a $HOME/notas ..."
  rm -rf "$HOME/notas.bak"
  if [ -d "$HOME/notas" ]; then
    mv "$HOME/notas" "$HOME/notas.bak"
  fi
  cp -r "$BASE_DIR/notas" "$HOME/notas"
fi

echo "Listo. Abrí Emacs para que termine de instalar paquetes."
