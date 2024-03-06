{
  programs.starship = {
    enable = true;
    settings = {
      # leading character of input
      character = {
        success_symbol = "[➜](bold)";
        error_symbol = "[➜](bold red)";
      };
      directory = {
        style = "cyan";
        truncation_length = 0;
        truncate_to_repo = false;
      };
      username = {
        # orange color
        style_user = "209";
        show_always = true;
      };
      git_branch = {
        # blue color
        style = "81";
      };
      git_status = {
        style = "none";
      };
      shell = {
        disabled = false;
      };
    };
  };
}

