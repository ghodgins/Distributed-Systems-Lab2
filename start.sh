#!/bin/bash
address=${1-127.0.0.1}
port=${2-8080}

./lab2-socketserver $address $port
