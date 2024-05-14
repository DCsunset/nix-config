{ lib }:

with lib;
rec {
  # read all modules in a directory as a list
  # dir should be a path
  readModules = dir: map
    (m: dir + "/${m}")
    (builtins.attrNames (
      filterAttrs
        # if it's a directory, it should contain default.nix
        (n: v: v == "directory" || strings.hasSuffix ".nix" n)
        (builtins.readDir dir)
    ));

  # list names of all subdirectories in a dir
  listSubdirNames = dir:
    builtins.attrNames (
      filterAttrs
        (n: v: v == "directory")
        (builtins.readDir dir)
    );

  # list paths of all subdirectories in a dir
  listSubdirPaths = dir: map (x: dir + "/${x}") (listSubdirNames dir);

  recursiveMergeAttrs = attrs: foldl' (acc: attr: recursiveUpdate acc attr) {} attrs;

  # import all modules in subdirs
  importSubdirs = dir: map import (listSubdirPaths dir);

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

  # Parse ip and len from format addr/len
  parseIp = subnet: let
    split = splitString "/" subnet;
    addr = builtins.elemAt split 0;
    len = builtins.elemAt split 1;
  in {
    inherit addr len;
  };

  # Convert ipv4 subnet/len or subnet to prefix like 10.0.0. (must end with .0)
  ipv4Prefix = subnet: removeSuffix "0" (builtins.elemAt (splitString "/" subnet) 0);

  # Use the prefix as gateway (without len) as it's not a special addr in IPv6
  # Convert ipv6 subnet/len to prefix like fdxx:: (must end with ::)
  ipv6Prefix = subnet: builtins.elemAt (splitString "/" subnet) 0;

  # Append a suffix to ipv4 subnet that ends with x.x.x.0/xx (len is kept)
  ipv4Append = subset: suffix: let
    ip = parseIp subset;
  in "${ipv4Prefix ip.addr}${suffix}/${ip.len}";

  # Append a suffix to ipv6 subnet that ends with ::/xx (len is kept)
  ipv6Append = subset: suffix: let
    ip = parseIp subset;
  in "${ip.addr}${suffix}/${ip.len}";

  # Check if it is ipv4 with optional len or optional port
  isIpv4 = addr: (builtins.match ''[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+((/[0-9]+)|(:[0-9]+))?'' addr) != null;

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

  findUdevRuleByName = name: udevRules:
    findFirst (x: x ? "name" && x.name == name) null udevRules;

  sandboxServiceConfig = {
    PrivateTmp = true;
    PrivateDevices = true;
    PrivateIPC = true;
    PrivateUsers = true;
    ProtectHostname = true;
    ProtectHome = true;
    ProtectClock = true;
    ProtectProc = "invisible";
    ProtectKernelTunables = true;
    ProtectKernelModules = true;
    ProtectKernelLogs = true;
    ProtectControlGroups = true;
    NoNewPrivileges = true;
    RestrictSUIDSGID = true;
    RestrictRealtime = true;
    LockPersonality = true;
    # remove all capabilities
    CapabilityBoundingSet = [ "" ];
    InaccessiblePaths = [ "/keys" "/mnt" "/data" "/boot" ];
    TemporaryFileSystem = [ "/var" "/run" "/etc" ];
    # For DNS and TLS
    BindReadOnlyPaths = [
      "/etc/ssl"
      "/etc/static/ssl"
      "/etc/pki"
      "/etc/static/pki"
      "/etc/resolv.conf"
    ];
  };
}

