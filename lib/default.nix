{ lib }:

with lib;
rec {
  # read all modules in a directory as a list
  # dir should be a path
  readModules = dir: map
    (m: dir + "//${m}")
    (builtins.attrNames (
      filterAttrs
        # if it's a directory, it should contain default.nix
        (n: v: v == "directory" || strings.hasSuffix ".nix" n)
        (builtins.readDir dir)
    ));

  # read multiple files and concat them into one string by newlines
  readFiles = files: concatStringsSep "\n" (
    map builtins.readFile files
  );

  importIfExist = paths: foldl'
    (r: p: if (builtins.pathExists p)
           then r ++ [ p ]
           else r)
    []
    paths;

  # for debug
  # importIfExist = paths: paths;
  getMachineConfig = machine:
    import (../machines + "/${machine}");

  # import preset modules and machine-specific module
  importPresets = machine: let
    cfg = getMachineConfig machine;
    presets = mapAttrsToList (n: v: import (../presets + "/${n}") v) cfg.presets;
  in {
    os = map (p: attrByPath ["os"] {} p) (presets ++ [ cfg ]);
    home = map (p: attrByPath ["home"] {} p) (presets ++ [ cfg ]);
  };

  # return a list of files to import for nixos
  importOsPresets = machine: let
    cfg = getMachineConfig machine;
  in importIfExist (
    (map (p: ../presets + "/${p}/os.nix") cfg.presets)
    ++ [ (../machines + "/${machine}/os.nix") ]
  );

  # return a list of files to import for home-manager
  importHomePresets = machine: let
    cfg = getMachineConfig machine;
  in importIfExist (
    map (p: ../presets + "/${p}/home.nix") cfg.presets
    ++ [ (../machines + "/${machine}/home.nix") ]
  );

  # Covert ipv4 subnet/len to gateway or gateway/len
  ipv4Gateway = subnet: withLen: let
    split = splitString "/" subnet;
    p = builtins.elemAt split 0;
    l = builtins.elemAt split 1;
    # replace .0 with .1)
    gateway = (removeSuffix ".0" p) + ".1";
  in (
    if withLen then "${gateway}/${l}" else gateway
  );

  # Parse ip and len from format ip/len
  parseIp = subnet: let
    split = splitString "/" subnet;
    ip = builtins.elemAt split 0;
    len = builtins.elemAt split 1;
  in {
    inherit ip;
    len = toInt len;
  };

  # Covert ipv4 subnet/len to prefix like 10.0.0. (must end with .0)
  ipv4Prefix = subnet: removeSuffix "0" (builtins.elemAt (splitString "/" subnet) 0);

  # Use the prefix as gateway (without len) as it's not a special addr in IPv6
  # Covert ipv6 subnet/len to prefix like 10.0.0. (must end with ::)
  ipv6Prefix = subnet: builtins.elemAt (splitString "/" subnet) 0;

  isIpv4Addr = addr: (builtins.match ''[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+(/[0-9]+)?'' addr) != null;

  # command to generate firejail opions to whitelist only the nix closure
  firejailNixClosure = pkgs: path: cmdLine: ''
    ${pkgs.nix}/bin/nix path-info --recursive ${path} | \
      ${if cmdLine
        then "${pkgs.gnused}/bin/sed 's/^/--whitelist=/' | ${pkgs.coreutils-full}/bin/tr '\n' ' '"
        else "${pkgs.gnused}/bin/sed 's/^/whitelist /'"}'';

  # command to generate firejail opions to whitelist only the nix closure
  bwrapNixClosure = pkgs: path: ''
    ${pkgs.nix}/bin/nix path-info --recursive ${path} | \
      ${pkgs.sd}/bin/sd -- '(.+)' '--ro-bind $1 $1'
  '';

  # use bwrap to run a target command
  bwrapCmd = { pkgs, command, devBind ? [], roBind ? [], bind ? [], extraOptions ? "" }: let
    buildBindOpts = opt: dirs:
      concatStrings (map (d: "${opt} ${d} ${d} ") dirs);
  in ''
    ${pkgs.bubblewrap}/bin/bwrap \
      ${buildBindOpts "--bind" bind} \
      ${buildBindOpts "--dev-bind" devBind} \
      ${buildBindOpts "--ro-bind" roBind} \
      ${extraOptions} \
      ${command}
  '';
}

