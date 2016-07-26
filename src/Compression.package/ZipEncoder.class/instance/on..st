initialization
on: aCollectionOrStream
	aCollectionOrStream isStream 
		ifTrue:[encodedStream := aCollectionOrStream]
		ifFalse:[	encodedStream := aCollectionOrStream writeStream].
	encodedStream isBinary
		ifTrue:[super on: (ByteArray new: 4096)]
		ifFalse:[super on: (String new: 4096)].
	bitPosition := bitBuffer := 0.