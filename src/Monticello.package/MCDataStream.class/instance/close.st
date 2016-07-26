other
close
	"Close the stream."

	| bytes |
	byteStream closed 
		ifFalse: [
			bytes := byteStream position.
			byteStream close]
		ifTrue: [bytes := 'unknown'].
	^ bytes