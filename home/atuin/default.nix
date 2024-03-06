{
  programs.atuin = {
    enable = true;
    flags = [
      "--disable-up-arrow"
    ];
    settings = {
      style = "compact";
      auto_sync = false;
      update_check = false;
    };
  };
}
