I am ZnStaticFileServerDelegate.
I am a simple proof of concept implementation of a web server serving static files.
I handle urls with an optional prefix as requests for files in a directory.
I serve index.html or index.htm when a directory is requested and these files exist.
I do a redirect when a path that is not does not end with a / refers to directory.
I function as a delegate for ZnServer.

ZnServer startDefaultOn: 1701.
ZnServer default delegate: ((ZnStaticFileServerDelegate new) 
									prefixFromString: 'apple/macosx'; 
									directory: '/Library/WebServer/Documents' asFileReference; 
									yourself).

Part of Zinc HTTP Components.