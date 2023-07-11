#! /usr/bin/env bash

siberia < default.cfg
test -f siberia-????.output
exit $?
