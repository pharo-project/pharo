collection ops
remove: aCharacter ifAbsent: aBlock
	(self includes: aCharacter) ifFalse: [^aBlock value].
	^self remove: aCharacter