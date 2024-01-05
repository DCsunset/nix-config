{
  programs.helix = {
    enable = true;

    # config.toml
    settings = {
      theme = "dark_plus";
      editor.cursor-shape.insert = "bar";
      keys = {
        normal = {
          # remap direction keys
          j = "move_char_left";
          k = "move_line_down";
          l = "move_line_up";
          ";" = "move_char_right";
          h = "collapse_selection";

          A-left = "jump_backward";
          A-right = "jump_forward";

          # goto mode
          g = {
            j = "goto_line_start";
            k = "goto_last_line";
            l = "goto_file_start";
            ";" = "goto_line_end";
            h = "no_op";
          };

          # View mode
          z = {
            k = "scroll_down";
            l = "scroll_up";
          };
        };

        insert = {
          A-i = "completion";
        };

        select = {
          # remap direction keys
          j = "extend_char_left";
          k = "extend_line_down";
          l = "extend_line_up";
          ";" = "extend_char_right";
          h = "collapse_selection";

          # goto mode
          g = {
            j = "goto_line_start";
            k = "goto_last_line";
            l = "goto_file_start";
            ";" = "goto_line_end";
            h = "no_op";
          };
        };
      };
    };

    # languages.toml
    languages = let
      commonLanguageConfig = {
        indent = {
          tab-width = 2;
          unit = "\t";
        };
        auto-format = false;
      };

      languages = [
        "markdown"
        "rust"
        "toml"
        "cpp"
        "c"
        "javascript"
        "typescript"
        "html"
        "python"
        "latex"
        "bash"
        "make"
        "git-config"
        "go"
      ];
    in {
      language = (
        map
          (l: commonLanguageConfig // {
            name = l;
          })
          languages
      );
    };
  };
}

