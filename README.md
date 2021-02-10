# dotfiles_rpi
System configuration files for my Raspberry Pi devices, including:
- Raspberry Pi 3 Model B
- Raspberry Pi Zero W
- (eventually) Raspberry Pi 4 Model B (4GB or 8GB RAM)

Essentially a public fork of my private repo git@github.com:felker/dotfiles_remote.git as of 2021-02-10.

Public so I can bypass any SSH keypair setup specific to my Raspberry Pi's and instead directly clone this repo when setting up a new device (or pushing changes back here). 

## Other Raspberry Pi OS and RealVNC "VNC Viewer" notes

- LXTerminal > Style > Palette > Solarized Dark
- Change default resolution 
- Change default shell to Zsh


In order to get a functional Meta key for Emacs over VNC Viewer, need to configure 
`LeftCmdKey` > `Super_L` (default is `Alt_L`)
`LeftOptKey` > `Alt_L` (default is `ExtendedChars`)

See [Keyboard Mapping To and From a Mac](https://help.realvnc.com/hc/en-us/articles/360002250597-Keyboard-Mapping-To-and-From-a-Mac#connecting-%E2%80%98new%E2%80%99-mac-to-%E2%80%98old%E2%80%99-mac-0-2)

**Unrelated**: on Karabiner on macOS, I added a mapping (in summer or fall 2020) from the PC application key (AKA "[menu key](https://en.wikipedia.org/wiki/Menu_key)") to [Fn](https://en.wikipedia.org/wiki/Fn_key) to match the custom macOS keycap layout on my Das Keyboard. This was so that I can control the debugger and builder steps in VSCode.

Left and Right Opt keys still can be used to produce extended characters on macOS. For example, Option+5 = ∞.

https://www.macworld.co.uk/how-to/mac-keyboard-type-symbols-3504584/

## Other topics I should eventually take note of
- Homebridge installation and setup
- ConBee II and deCONZ setup
- Unifi Protect plugin for Homebridge
- Linking deCONZ and Homebridge services to account for the delay in bringing up the lights resources in the Zigbee network before Homebridge is started
- OTAU for IKEA Tradfri firmware
- How to update the correct Node and npm executables and libraries that match the Homebridge distro
