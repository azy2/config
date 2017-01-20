#!/usr/bin/env sh

killall -q polybar

while pgrep -x polybar >/dev/null; do sleep 1; done

polybar primary& &> /dev/null
polybar secondary& &> /dev/null
