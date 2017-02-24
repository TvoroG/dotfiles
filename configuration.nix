# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  callPackage = pkgs.lib.callPackageWith (pkgs // pkgs.xlibs // mypkgs);
  mypkgs = {
    popcorntime = callPackage ./pkgs/applications/video/popcorntime {
      nwjs = pkgs.nwjs_0_12;
      gconf = pkgs.gnome2.GConf;
      gtk = pkgs.gtk2;
    };
  };
in {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sdb";

  boot.extraModprobeConfig =
  ''
  options rtl8723be fwlps=N ips=N
  '';

  networking.hostName = "tvorog"; # Define your hostname.
  networking.networkmanager = {
    enable = true;
  };

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
    supportedLocales = ["en_US.UTF-8/UTF-8" "ru_RU.UTF-8/UTF-8"];
  };

  # Set your time zone.
  time.timeZone = "Europe/Moscow";

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  programs.ssh.startAgent = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  boot.blacklistedKernelModules = ["nouveau"];

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.exportConfiguration = true;
  services.xserver.videoDrivers = ["nvidia" "intel"];

  hardware.bumblebee.enable = true;
  hardware.bumblebee.driver = "nvidia";
  hardware.bumblebee.connectDisplay = true;

  services.xserver.layout = "us,ru";
  services.xserver.xkbOptions = "grp:alt_shift_toggle, ctrl:nocaps";
  services.xserver.synaptics.enable = true;
  services.xserver.synaptics.vertEdgeScroll = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.windowManager.default = "xmonad";
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.displayManager.slim.defaultUser = "marsel";
  services.xserver.displayManager.sessionCommands =
    ''
      xrdb -merge .Xdefaults
      feh --bg-scale .background.png
      xsetroot -cursor_name left_ptr
    '';

  services.ntp.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.marsel = {
    isNormalUser = true;
    uid = 1000;
    home = "/home/marsel";
    group = "marsel";
    extraGroups = [ "wheel" "networkmanager" "audio" "video" "docker" "vboxusers"];
   # openssh.authorizedKeys.keyFiles = ["/home/marsel/.ssh/id_rsa.pub" "/home/marsel/.ssh/lab_id_rsa.pub"];
  };
  users.extraGroups.marsel.gid = 1000;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

  # services.logind.extraConfig = "HandleLidSwitch=ignore";

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.systemWide = true;

  programs.zsh.enable = true;
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  networking.extraHosts = ''
  127.0.0.1 dev.24smi.org dev.glavnoe.io
  127.0.0.1 gitlab
  127.0.0.1 smi.net
  '';

  services.tlp.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
  };

  services.printing = {
    enable = true;
    drivers = [ pkgs.splix ];
  };

  services.emacs.enable = true;
  services.emacs.defaultEditor = true;

  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.guest.enable = true;

  # services.offlineimap.enable = true;

  nixpkgs.config.chromium = {
    # enablePepperFlash = true;
    enablePepperPDF = true;
  };

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    ansible2 = super.ansible2.overrideDerivation (old: rec {
      version = "2.2.0.0";
      name = "ansible-${version}";
      src = pkgs.fetchurl {
        url = "http://releases.ansible.com/ansible/${name}.tar.gz";
        sha256 = "11l5814inr44ammp0sh304rqx2382fr629c0pbwf0k1rjg99iwfr";
      };
    });
  };

  nix.nixPath = [ "/home/marsel/programming" "nixpkgs=/home/marsel/programming/nixpkgs" "nixos-config=/etc/nixos/configuration.nix" ];

  environment.systemPackages = with pkgs; [
    binutils
    file
    gcc
    htop
    xclip
    bashInteractive
    usbutils
    glibc_multi
    bridge-utils
    openssl
    gnumake

    hibernate
    cpufrequtils
    xorg.xbacklight
    xorg.xev
    xscreensaver

    nox
    nix-repl
    dmenu2
    irssi
    manpages
    p7zip
    unzip
    unrar
    zip
    wget
    which
    telnet
    bind
    tcpflow
    wireshark
    wireshark-gtk
    nmap
    sysstat
    iotop
    dstat
    graphviz
    ffmpeg

    haskellPackages.xmobar
    geeqie
    evince
    feh
    mc
    silver-searcher
    lm_sensors
    hddtemp
    pciutils
    hwinfo
    pavucontrol
    libnotify
    notify-osd
    gnupg
    gnupg1
    sqlite
    msmtp
    notmuch
    graphicsmagick
    neomutt
    urlview
    imgur-screenshot
    xfce.terminal
    octave

    vim
    source-code-pro

    vagrant
    kubernetes

    rxvt_unicode-with-plugins
    vlc
    gitFull
    mercurialFull
    chromium
    firefox
    opera
    transmission_gtk
    cmus
    weechat
    skype
    spotify
    dropbox-cli

    ghc
    go
    gocode
    rustc
    cargo

    # pypy
    python27
    python33
    python34
    python35
    python35Packages.flake8
    python35Packages.setuptools
    python35Packages.youtube-dl
    python27Packages.docker_compose
    python35Packages.virtualenvwrapper
    python35Packages.virtualenv
    python27Packages.setuptools
    ansible2
    libyaml
    zlib

    mypkgs.popcorntime
  ];
}
