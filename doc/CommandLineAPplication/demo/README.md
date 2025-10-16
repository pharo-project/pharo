# Some demo scripts to generate animated gifs and snapshots

Scripts (demos) are written in tap files.
It uses [VHS](https://github.com/charmbracelet/vhs/blob/main/README.md) tool to generate images.


## Hello demo
Write a small hello wrapper script to run the pharo image with the hello command.

```bash
#!/usr/bin/env bash

# some magic to find out the real location of this script dealing with symlinks
DIR=`readlink "$0"` || DIR="$0";
DIR=`dirname "$DIR"`;

"$DIR"/Pharo.app/Contents/MacOs/Pharo --headless "$DIR"/Pharo.image --no-default-preferences hello "$@"
```

Run the tap file:
```bash
vhs hello-world.tape
```

Copy generated images in `CommandLineAPplication/image`