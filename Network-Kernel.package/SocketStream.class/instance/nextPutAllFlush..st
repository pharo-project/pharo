stream out
nextPutAllFlush: aCollection
	"Put a String or a ByteArray onto the stream.
	You can use this if you have very large data - it avoids
	copying into the buffer (and avoids buffer growing)
	and also flushes any other pending data first."

	| toPut |
	toPut := binary ifTrue: [aCollection asByteArray] ifFalse: [aCollection asString].
	self flush. "first flush pending stuff, then directly send"
	socket isOtherEndClosed ifFalse: [
		[socket sendData: toPut count: toPut size]
			on: ConnectionTimedOut
			do: [:ex | shouldSignal ifFalse: ["swallow"]]]