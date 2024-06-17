{ config, pkgs, ... }:

let
  user = config.home.username;

  cspellCustomDict = "/home/${user}/.config/cspell/custom-dict.txt";
  cspellConfig = {
    version = "0.2";
    dictionaries = [ "custom-dict" ];

    languageSettings = [
      {
        languageId = "org";
        dictionaries = [ "gaming-terms" "elisp" ];
      }
    ];

    dictionaryDefinitions = [
      # custom-dict can be modified at any time
      {
        name = "custom-dict";
        path = cspellCustomDict;
      }
    ];

    import = [
      "${pkgs.nur-dcsunset.cspellDicts.elisp}/share/cspell-dict-elisp/cspell-ext.json"
    ];
  };
in
{
  # create custom dict if not exists
  systemd.user.tmpfiles.rules = [
    "f ${cspellCustomDict} - - - -"
  ];

  # global config for cspell
  home.file.".config/configstore/cspell.json".text = builtins.toJSON cspellConfig;

  home.packages = with pkgs; [
    nodePackages.cspell
  ];
}

