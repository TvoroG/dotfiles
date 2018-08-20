# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
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
  # networking.nameservers = [ "127.0.0.1" ];
  networking.networkmanager = {
    enable = true;
    # useDnsmasq = true;
  };

  # services.dnsmasq = {
  #   enable = true;
  #   # resolveLocalQueries = true;
  #   servers = [ "8.8.8.8" "8.8.4.4" ];
  #   extraConfig = ''
  #   listen-address=127.0.0.1
  #   '';
  # };

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

  boot.blacklistedKernelModules = ["nouveau"];

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.exportConfiguration = true;
  services.xserver.videoDrivers = ["nvidia" "intel"];

  services.xserver.layout = "us,ru";
  services.xserver.xkbOptions = "grp:alt_shift_toggle, ctrl:nocaps";
  services.xserver.synaptics.enable = true;
  services.xserver.synaptics.vertEdgeScroll = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.windowManager.default = "xmonad";
  # services.xserver.windowManager.exwm.enable = true;
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.displayManager.slim.defaultUser = "marsel";
  services.xserver.displayManager.sessionCommands =
    ''
      xrdb -merge .Xdefaults
      feh --bg-scale .background.png
      xsetroot -cursor_name left_ptr
    '';

  services.ntp = {
    enable = true;
    servers = [
      "ntp2.stratum2.ru"
      "ntp3.stratum2.ru"
      "ntp4.stratum2.ru"
      "ntp5.stratum2.ru"
    ];
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.marsel = {
    isNormalUser = true;
    uid = 1000;
    home = "/home/marsel";
    group = "marsel";
    extraGroups = [ "wheel" "networkmanager" "audio" "video" "docker" "vboxusers"];
  };
  users.extraGroups = {
    marsel = {
      gid = 1000;
    };
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.03";

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.systemWide = true;
  sound.enable = true;

  programs.zsh.enable = true;
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  services.tlp.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
    virtualbox.enableExtensionPack = true;
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

  fonts.fonts = [pkgs.hack-font];

  nixpkgs.config.chromium = {
    # enablePepperFlash = true;
    enablePepperPDF = true;
  };

  nix.nixPath = [ "/home/marsel/programming" "nixpkgs=/home/marsel/programming/nixpkgs" "nixos-config=/etc/nixos/configuration.nix" ];
  nix.binaryCaches = [ "https://cache.nixos.org/" ];

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
    bash
    autoconf

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
    tcpdump
    traceroute
    sysstat
    iotop
    dstat
    graphviz
    ffmpeg
    openvpn
    gnome3.networkmanagerapplet
    gnome3.networkmanager_openvpn
    update-resolv-conf

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
    byzanz
    xorg.xwininfo
    electron

    vim
    source-code-pro
    ctags

    vagrant

    vlc
    gitFull
    chromium
    firefox
    transmission_gtk
    spotify
    dropbox-cli
    calibre

    ghc
    go
    gocode
    rustc
    cargo
    lua
    elixir

    # pypy
    python27
    python36
    python36Packages.pyqt5
    python36Packages.flake8
    python36Packages.setuptools
    python36Packages.youtube-dl
    python27Packages.docker_compose
    python36Packages.virtualenvwrapper
    python36Packages.virtualenv
    python27Packages.setuptools
    ansible2
    libyaml
    zlib
    
    nodejs-8_x
    yarn

    networkmanager-l2tp
  ];
}
