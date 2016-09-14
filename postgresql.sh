#!/bin/sh

# PostgreSQL

initdb -D /usr/local/var/postgres
createdb `whoami`
psql -d `whoami` -f priv/init.sql
