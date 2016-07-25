ZnManagingMultiThreadedServer is a ZnMultiThreadedServer that manages the socket stream connections used by the worker threads that are spawned.

  ZnManagingMultiThreadedServer startDefaultOn: 1701.

I keep track of all the connections that I spawn so that I can close them when I am stopped.

Part of Zinc HTTP Components.