xsetroot -cursor_name left_ptr&
setxkbmap -layout dvorak
xmodmap /home/ben/config/dvorak
xset r rate 200 50

trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --height 38 --transparent true --alpha 0 --tint 0x242424 --monitor primary &

pasystray &

dropbox &

feh --bg-scale ~/Documents/deer.jpg

# xrandr --output HDMI-0 --scale 2x2 --mode 1920x1080 --fb 5760x2160 --pos 3840x0
# xrandr --output DP-2 --scale 1x1 --mode 3840x2160 --pos 0x0
# xrandr --output DP-2 --primary --left-of HDMI-0

[[ -f ~/config/rofi.conf ]] && xrdb -merge -I$HOME ~/config/rofi.conf

xrandr --output DP-2 --auto --primary --output HDMI-0 --off
