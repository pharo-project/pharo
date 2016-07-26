decorated
basicNextChunk
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character."

	| out ch |
	out := (String new: 1000) writeStream.
	self skipSeparators.
	[ (ch := decoratedStream next) isNil ]
		whileFalse: [ 
			ch == self terminatorMark
				ifTrue: [ 
					decoratedStream peek == self terminatorMark
						ifTrue: [ decoratedStream next	"skip doubled terminator" ]
						ifFalse: [ ^ out contents	"terminator is not doubled; we're done!" ] ].
			out nextPut: ch ].
	^ out contents