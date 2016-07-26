I am ZnServer, an abstract superclass of HTTP Servers.
I am a facade for controlling a default server implementation.

I delegate my public class protocol methods to #defaultServerClass.

  ZnServer startDefaultOn: 1701.
  ZnClient new get: 'http://localhost:1701'.

Subclasses can register with me to have start/stop sent to them on System startUp/shutDown.
The default server instance will be registered automatically when it is started.

Part of Zinc HTTP Components.