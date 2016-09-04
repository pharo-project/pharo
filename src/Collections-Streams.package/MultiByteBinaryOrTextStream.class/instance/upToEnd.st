accessing
upToEnd

	| newStream element newCollection |
	newCollection := self isBinary
				ifTrue: [ByteArray new: 100]
				ifFalse: [String new: 100].
	newStream := newCollection writeStream.
	[(element := self next) notNil]
		whileTrue: [newStream nextPut: element].
	^ newStream contents
