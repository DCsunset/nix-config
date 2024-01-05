{ osConfig, lib, pkgs, ... }:

with lib;
mkIf osConfig.custom.graphics.gui {
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;

    extensions = with pkgs.vscode-extensions; [
      llvm-vs-code-extensions.vscode-clangd
      streetsidesoftware.code-spell-checker
      foam.foam-vscode
      ms-vscode.hexeditor
      mads-hartmann.bash-ide-vscode
      eamodio.gitlens
      james-yu.latex-workshop
      yzhang.markdown-all-in-one
      ms-python.python
      mechatroner.rainbow-csv
      matklad.rust-analyzer
      gruntfuggly.todo-tree
      golang.go
    ];

    userSettings = {
      # Editor config
      "update.mode" = "none";  # disable auto update
      "editor.wordWrap" = "on";
      "editor.insertSpaces" = false;
      "editor.tabSize" = 2;
      "editor.wrappingIndent" = "none";
      "editor.cursorBlinking" = "solid";
      "editor.bracketPairColorization.enabled" = false;
      "editor.tabCompletion" = "on";
      "terminal.external.linuxExec" = "alacritty";

      # Language specific
      ## for ESM module to work
      "javascript.preferences.importModuleSpecifierEnding" = "js";
      "typescript.preferences.importModuleSpecifierEnding" = "js";

      # Extensions
      ## vscode-modal-editor
      "modalEditor.misc.ignoreUndefinedKeys" = true;
      "modalEditor.misc.keybindingsInSettings" = false;
      "modalEditor.misc.autoloadPreset" = "helix.js";
      ## Set preset dir in nix-store
      "modalEditor.misc.presetDirectory" = "${./vscode-modal-editor}";
      "modalEditor.styles".insert.cursorStyle = "line";
      ## Code Spell Checker
      "cSpell.dictionaries" = [
        "bash"
        "cpp-refined"
        "cryptocurrencies"
        "css"
        "docker"
        "filetypes"
        "fullstack"
        "golang"
        "latex"
        "node"
        "npm"
        "public-licenses"
        "python"
        "rust"
        "sql"
        "softwareTerms"
        "networking-terms"
        "typescript"
      ];
    };

    keybindings = [
      {
        key = "alt+[";
        command = "workbench.action.togglePanel";
      }
      {
        key = "alt+]";
        command = "workbench.action.terminal.focus";
        when = "!terminalFocus";
      }
      {
        key = "alt+]";
        command = "workbench.action.focusActiveEditorGroup";
        when = "terminalFocus";
      }
      {
        key = "ctrl+t";
        command = "workbench.action.terminal.new";
        when = "terminalFocus";
      }
      {
        key = "ctrl+c";
        command = "editor.action.commentLine";
        when = "editorTextFocus && !editorReadonly";
      }
      {
        key = "ctrl+e";
        command = "-workbench.action.quickOpen";
      }
      {
        key = "ctrl+i";
        command = "workbench.action.navigateForward";
        when = "editorTextFocus";
      }
      {
        key = "ctrl+o";
        command = "workbench.action.navigateBack";
        when = "editorTextFocus";
      }
      {
        key = "ctrl+w";
        command = "-workbench.action.closeActiveEditor";
      }
      {
        key = "ctrl+f4";
        command = "workbench.action.closeActiveEditor";
      }
      {
        key = "ctrl+q";
        command = "-workbench.action.quit";
      }
      {
        key = "ctrl+q";
        command = "workbench.action.closeWindow";
      }
      {
        key = "ctrl+w";
        command = "deleteWordLeft";
        when = "textInputFocus && modalEditor.mode == 'insert'";
      }

      # vscode-modal-editor
      {
        key = "ctrl+u";
        command = "modalEditor.halfPageUp";
      }
      {
        key = "ctrl+d";
        command = "modalEditor.halfPageDown";
      }
      {
        key = "enter";
        command = "-editor.action.nextMatchFindAction";
        when = "editorFocus && findInputFocussed";
      }
      {
        key = "shift+enter";
        command = "-editor.action.previousMatchFindAction";
        when = "editorFocus && findInputFocussed";
      }
      {
        key = "enter";
        command = "modalEditor.executeCommand";
        args = [
          "workbench.action.focusActiveEditorGroup"
          {
            command = "cursorLeftSelect";
            when = "!_ctx.selection.isEmpty";
          }
        ];
        when = "editorFocus && findInputFocussed";
      }
      {
        key = "ctrl+a";
        command = "editor.emmet.action.incrementNumberByOne";
        when = "textInputFocus";
      }
      {
        key = "ctrl+x";
        command = "editor.emmet.action.decrementNumberByOne";
        when = "textInputFocus";
      }
      {
        key = "alt+.";
        command = "modalEditor.replayRecord";
        args = "motion";
        when = "textInputFocus";
      }
      {
        key = "ctrl+w";
        command = "deleteWordLeft";
        when = "textInputFocus && !editorReadonly && modalEditor.mode == 'insert'";
      }
      {
        key = "ctrl+w";
        command = "deleteWordLeft";
        when = "textInputFocus && !editorFocus";
      }
      {
        key = "backspace";
        command = "-deleteLeft";
        when = "editorFocus";
      }
      {
        key = "backspace";
        command = "-deleteLeft";
        when = "textInputFocus";
      }
      {
        key = "backspace";
        command = "deleteLeft";
        when = "textInputFocus && modalEditor.mode == 'insert'";
      }
      {
        key = "backspace";
        command = "deleteLeft";
        when = "textInputFocus && !editorFocus";
      }
      {
        key = "ctrl+n";
        command = "selectNextSuggestion";
        when = "suggestWidgetVisible && textInputFocus";
      }
      {
        key = "ctrl+p";
        command = "selectPrevSuggestion";
        when = "suggestWidgetVisible && textInputFocus";
      }
    ];
  };
}

