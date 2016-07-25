public - string reading
protocolAt: sourcePointer for: sourceDataPointers
	"Answer the protocol for a given method, retrieved from the sources or changes file. Answer unfound protocol if no protocol is available."

	| preamble protocol tokens protocolPosition |
	protocol := 'unfound protocol'.	"this is to indicate that the tagging in the source does not use the correct format.
	We will have to fix that. For example some traits methods are wrongly tagged.
	see http://code.google.com/p/pharo/issues/detail?id=4581"
	preamble := self sourcedDataAt: sourcePointer.
	preamble = 'Trait method' ifTrue: [ ^ nil ].
	(preamble includesSubstring: sourceDataPointers key) ifTrue: [ 
		tokens := preamble parseLiterals.
		protocolPosition := tokens indexOf: sourceDataPointers key.
		protocolPosition = 0
			ifFalse: [ 
				"New format gives change protocol and unified prior pointer"
				protocol := tokens at: protocolPosition + 1 ] ].
	^ protocol