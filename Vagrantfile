Vagrant.configure(2) do |config|
  config.vm.box = "debian/jessie64"
  config.vm.provider "virtualbox" do |v|
    v.cpus = 4
  end
  config.vm.provision "shell", inline: <<-SHELL
    sudo apt-get update
    wget https://github.com/ralt/dh-quicklisp-buildapp/releases/download/0.5/dh-quicklisp-buildapp_0.5_all.deb
    sudo dpkg -i dh-quicklisp-buildapp_0.5_all.deb || true
    sudo apt-get -f install -y
  SHELL
end
