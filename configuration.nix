# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let npkgs = import <nixpkgs> {};
in {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  fileSystems."/home" = {
    device = "/dev/disk/by-label/home";
    fsType = "ext4";
  };

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sdb";

  networking.hostName = "tvorog"; # Define your hostname.
  networking.networkmanager.enable = true;

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
    openssh.authorizedKeys.keyFiles = ["/home/marsel/.ssh/id_rsa.pub" "/home/marsel/.ssh/lab_id_rsa.pub"];
  };
  users.extraGroups.marsel.gid = 1000;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

  # services.logind.extraConfig = "HandleLidSwitch=ignore";

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.systemWide = true;

  programs.zsh.enable = true;
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  networking.extraHosts = "127.0.0.1 dev.24smi.org";

  services.tlp.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
  };

  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.guest.enable = true;

  nixpkgs.config.chromium = {
    enablePepperFlash = true;
    enablePepperPDF = true;
  };

  environment.systemPackages = with pkgs; [
    binutils
    file
    gcc
    htop
    xclip
    bashInteractive
    usbutils
    glibc_multi

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
    wget
    which

    haskellPackages.xmobar
    geeqie
    feh
    mc
    silver-searcher
    lm_sensors
    pciutils
    hwinfo
    pavucontrol
    gnupg1
    graphicsmagick

    emacs
    vim
    source-code-pro

    vagrant

    rxvt_unicode-with-plugins
    vlc
    gitFull
    chromium
    transmission_gtk
    cmus
    weechat
    skype

    ghc

    python27Full
    python35
    python35Packages.flake8
    python35Packages.setuptools
    python27Packages.ansible
    python27Packages.virtualenvwrapper
    python27Packages.setuptools
    libyaml
    zlib
  ];
}
