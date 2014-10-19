#!/bin/bash

BUILD_DIR="$1"
mv $BUILD_DIR/potato-server/{.,}* $BUILD_DIR
rmdir $BUILD_DIR/potato-server
