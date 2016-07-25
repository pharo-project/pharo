I am ZnSingleThreadedServer.
I can be customized with a delegate (#handleRequest:) and an authenticator (#authenticateRequest:do:).

  ZnSingleThreadedServer startDefaultOn: 1701.
  ZnSingleThreadedServer default authenticator: (ZnBasicAuthenticator username: 'foo' password: 'secret').
  ZnClient new username: 'foo' password: 'secret'; get: 'http://localhost:1701'.

I use ZnDefaultServerDelegate when no other delegate is set.
I am single threaded, I run in a single process.
I close connections after each request/response cycle.

Part of Zinc HTTP Components.