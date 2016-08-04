# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
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
  # boot.loader.grub.device = "/dev/sda";

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

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  # environment.systemPackages = with pkgs; [
  #   wget
  # ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  programs.ssh.startAgent = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us,ru";
  services.xserver.xkbOptions = "grp:alt_shift_toggle, ctrl:nocaps";
  services.xserver.synaptics.enable = true;
  services.xserver.synaptics.vertEdgeScroll = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.windowManager.default = "xmonad";
  services.xserver.desktopManager.xterm.enable = false;
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
    extraGroups = [ "wheel" "networkmanager" "audio" "video"];
    openssh.authorizedKeys = {keyFiles = ["/home/marsel/.ssh/id_rsa.pub"];};
  };
  users.extraGroups.marsel.gid = 1000;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

  programs.zsh.enable = true;
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  nixpkgs.config = {
    allowUnfree = true;
  };

  environment.systemPackages = with pkgs; [
    binutils
    file
    gcc
    htop

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

    emacs
    vim
    source-code-pro

    rxvt_unicode
    vlc
    gitFull
    chromium
  ];
}
