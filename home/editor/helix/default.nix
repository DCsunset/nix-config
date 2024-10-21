{ config, lib, dc-lib, ... }:

with lib;
{
  programs.helix = {
    enable = true;

    # config.toml
    settings = {
      theme = "modus_vivendi";
      editor = {
        cursor-shape.insert = "bar";
        soft-wrap.enable = true;
      };
      keys = let
        commonKeys = {
          C-s = ":w";
          C-q = ":q";
          "C-;" = "goto_next_buffer";
          C-j = "goto_previous_buffer";
        };
        nonInsertKeys = {
          # goto mode
          g = {
            j = "goto_line_start";
            ";" = "goto_line_end";
          };

          # space mode
          space = {
            c = {
              y = "yank_to_clipboard";
              p = "paste_clipboard_after";
              P = "paste_clipboard_before";
            };
            # lsp
            l = {
              r = "rename_symbol";
              a = "code_action";
            };
            p = {
              f = "file_picker";
            };
            d = {
              f = "file_picker_in_current_directory";
            };
            r = ":reload";
          };

          # transformation
          "`" = {
            c = {
              l = "switch_to_lowercase";
              u = "switch_to_uppercase";
            };
          };

          # structural ops
          s = {
            e = {
              ";" = "goto_next_diag";
              j = "goto_prev_diag";
            };
          };

          c = "change_selection_noyank";
          d = "delete_selection_noyank";
          Q = ":buffer-close";
          "{" = "jump_backward";
          "}" = "jump_forward";
          A-s = "save_selection";
        };
      in {
        normal = dc-lib.recursiveMergeAttrs [
          commonKeys
          nonInsertKeys
          {
            j = "move_char_left";
            k = "move_visual_line_down";
            l = "move_visual_line_up";
            ";" = "move_char_right";
            K = "move_line_down";
            L = "move_line_up";
            C-u = replicate 10 "move_visual_line_up";
            C-d = replicate 10 "move_visual_line_down";
          }
        ];

        select = dc-lib.recursiveMergeAttrs [
          commonKeys
          nonInsertKeys
          {
            j = "extend_char_left";
            k = "extend_visual_line_down";
            l = "extend_visual_line_up";
            ";" = "extend_char_right";
            K = "extend_line_down";
            L = "extend_line_up";
            C-u = replicate 10 "extend_visual_line_up";
            C-d = replicate 10 "extend_visual_line_down";
          }
        ];

        insert = dc-lib.recursiveMergeAttrs [
          commonKeys
          {
            A-i = "completion";
          }
        ];
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

