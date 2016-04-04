xsetroot -cursor_name left_ptr
setxkbmap -layout dvorak
xmodmap /home/ben/config/dvorak
sudo rfkill unblock all
xset r rate 200 50
feh --bg-scale /home/ben/Documents/snev7Ga.png

MONITOR=`xrandr | grep -o "HDMI1 connected"`
if [ -n "$MONITOR" ]
then
    xrandr --output HDMI1 --auto --primary --left-of eDP1
else
    xmodmap ~/config/laptop
fi
