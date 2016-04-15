xsetroot -cursor_name left_ptr
setxkbmap -layout dvorak
xmodmap /home/ben/config/dvorak
sudo rfkill unblock all
xset r rate 200 50
feh --bg-scale /home/ben/Documents/dks3.png

trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --height 27 --transparent true --alpha 0 --tint 0x242424 &

pasystray &

nm-applet --sm-disable &

MONITOR=`xrandr | grep -o "HDMI1 connected"`
if [ -n "$MONITOR" ]
then
    xrandr --output HDMI1 --auto --primary --left-of eDP1
else
    xmodmap ~/config/laptop
fi
