I am ZnDefaultServerDelegate.
I function as a delegate for ZnServer, implementing #handleRequest:

I implement responses to the following prefixes:

/echo - an echo text of request information for debugging purposes
/dw-bench - a dynamic html page for benchmarking purposes
/unicode - a unicode test page
/random - a random string (/random/32 for a specific size)
/bytes - bytes according to a pattern (/bytes/32 for a specific size)
/favicon.ico - a Zn favicon
/status - a server status page
/error - force server errors
/session - a simpler session based counter
/help - lists all page prefixes
/ - an html welcome page

Without any matches, I respond with a page not found.

I can be reused without any default handlers mapped using #empty and configured using #map:to: where the second argument can be a block.

Part of Zinc HTTP Components.