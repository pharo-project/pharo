I am ZnMonticelloServerDelegate.
I am a proof of concept implementation of a basic Monticello server.
I function as a delegate for ZnServer.

ZnServer startDefaultOn: 1701.
ZnServer default delegate: ((ZnMonticelloServerDelegate new) 
									directory: '/tmp/monticello'; 
									yourself).

Part of Zinc HTTP Components.