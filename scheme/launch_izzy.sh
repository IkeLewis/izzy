#!/bin/bash
cd ~/scheme
echo -n auto > /sys/bus/serio/devices/serio0/bind_mode
echo -n serio_raw > /sys/bus/serio/devices/serio0/drvctl
g izzy/izzy.scm
