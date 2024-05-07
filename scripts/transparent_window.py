#! /usr/bin/env python3
import gi

gi.require_version("Gdk", "3.0")
gi.require_version("Gtk", "3.0")
gi.require_version("Wnck", "3.0")

from gi.repository import Gdk
from gi.repository import Gtk
from gi.repository import GLib
from gi.repository import Wnck

import sys
import subprocess
import re


class Monitor:
    def __init__(self, name, size, idx):
        self.name = name
        self.size = size
        self.idx = idx

    def __str__(self):
        return f"{self.name} {self.size} {self.idx}"

# use xrandr to get the screen size
xrandr = subprocess.Popen(['xrandr', '--listmonitors'], stdout=subprocess.PIPE)
xrandr_output = xrandr.communicate()[0].decode('utf-8')
xrandr_output = xrandr_output.split('\n')

monitors = []

for line in xrandr_output:
    # skip line if it doesn't start with a monitor id
    m = re.search(r'(\d+):', line)
    if not m:
        continue
    monitor_id = int(m.group(1))
    # monitor name is last item in line
    print(line)
    monitor_name = line.split(' ')[-1]
    print(monitor_name)
    screen_size = re.search(r'(\d+)/\d+x(\d+)', line)
    screen_position = re.search(r'\+(\d+)\+(\d+)', line)
    if screen_size:
        screen_size = (int(screen_size.group(1)), int(screen_size.group(2)))
    else:
        raise ValueError("Could not find screen size")
    if screen_position:
        screen_position = (int(screen_position.group(1)), int(screen_position.group(2)))
    else:
        raise ValueError("Could not find screen position")

    monitors.append(Monitor(monitor_name, screen_size, monitor_id))

def display_monitor_info(monitor):
    CSS = b"""
    @define-color color-a rgba(0, 0, 0, 0.5);
    @define-color color-b rgba(0, 0, 255, 0.5);
    @define-color full-transparent rgba(150, 150, 150, 0.2);
    #toplevel {
        background-color: transparent;
        background-image: radial-gradient(ellipse at center, @full-transparent %spx, @color-a %spx);
        font-size: 50px;
        color: #fff;
        text-shadow: 5px 5px 5px #000;
    }
    """ % (str(monitor.size[0] // 2 - 1).encode('utf-8'), str(monitor.size[0] // 2).encode('utf-8'))

    style_provider = Gtk.CssProvider()
    style_provider.load_from_data(CSS)

    Gtk.StyleContext.add_provider_for_screen(
        Gdk.Screen.get_default(),
        style_provider,
        Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION
    )

    button1 = Gtk.Button(label="Hello, world!")
    button1.set_margin_bottom(50)
    button1.set_margin_top(50)

    text = Gtk.Label(label=monitor.name)

    box = Gtk.Box(spacing=50)
    # box.pack_start(button1, True, True, 50)
    box.pack_start(text, True, True, 50)

    window = Gtk.Window(title=monitor.name, name="toplevel", type=Gtk.WindowType.TOPLEVEL)
    window.set_can_focus(False)
    window.set_can_default(False)
    window.set_decorated(False)
    # window.
    screen = window.get_screen()
    visual = screen.get_rgba_visual()
    window.set_visual(visual)
    window.add(box)
    window.show_all()
    window.fullscreen_on_monitor(Gdk.Screen.get_default(), monitor.idx)
    window.connect("destroy", Gtk.main_quit)

def move_helm_to_top():
    screen = Wnck.Screen.get_default()
    screen.force_update()
    for w in screen.get_windows():
        if w.get_name() == "Helm":
            if w.is_above() and w.is_active():
                return
            w.make_above()
            w.activate(0)

    GLib.timeout_add_seconds(0.1, move_helm_to_top)

for monitor in monitors:
    display_monitor_info(monitor)
move_helm_to_top()
Gtk.main()
