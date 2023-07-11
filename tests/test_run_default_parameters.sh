#! /usr/bin/env bash

siberia < default.cfg
test -f siberia-0001.output
exit $?
