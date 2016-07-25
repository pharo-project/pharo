configuration
shouldSignal: aBoolean
	"If shouldSignal is enabled the Socket Exceptions
	ConnectionClosed and ConnectionTimedOut will not be swallowed.
	Default is true. And please - don't set it to false - it is better to
	use an exception handler (see below)  and several methods
	in this class will not honour timeouts (says so in their method comments).
	Also, it is quite hard to understand what for example #upToEnd
	should return to indicate a timeout.
	
	Wrap your use of SocketStream with a handler like:
	
	[stuff := mySocketStream next: 10]
		on: ConnectionClosed, ConnectionTimedOut
		do: [:ex |
			Transcript show: 'Oops! Did not get my ten bytes!;cr]
	"

	shouldSignal := aBoolean