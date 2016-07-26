accessing
indexOf: aCharacter  startingAt: start  ifAbsent: aBlock
	| ans |
	(aCharacter isCharacter) ifFalse: [ ^ aBlock value ].
	ans := self class indexOfAscii: aCharacter asciiValue inString: self  startingAt: start.
	ans = 0
		ifTrue: [ ^ aBlock value ]
		ifFalse: [ ^ ans ]