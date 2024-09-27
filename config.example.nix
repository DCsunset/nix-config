{
  system = "x86_64-linux";
  user = "user";
  modules = [{
    config.custom.gui = {
      enable = false;
      displayServer = "x11";
    };
  }];
}
