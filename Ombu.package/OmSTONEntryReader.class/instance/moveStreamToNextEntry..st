reading
moveStreamToNextEntry: aStream

	[aStream upToAll: 'OmEntry {'.
	aStream peek = $\] whileTrue. "here we handle case when log contains this method itself"

	^aStream atEnd 
		ifTrue: [ -1 ]
		ifFalse: [ aStream position - 'OmEntry {' size]