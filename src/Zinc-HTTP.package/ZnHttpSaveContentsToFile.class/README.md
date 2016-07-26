I am ZnHttpSaveContentsToFile.
I am a ZnUrlOperation.

I implement the #saveContentsToFile: URL operation for HTTP(S) urls, which downloads the url resource to a file.

  'http://zn.stfx.eu/zn/numbers.txt' asZnUrl saveContentsToFile: Path * 'numbers.txt'.

Part of Zinc HTTP Components.