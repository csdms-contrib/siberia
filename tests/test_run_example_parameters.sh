#! /usr/bin/env bash

siberia < example.cfg
test -f siberia-0002.output
exit $?
