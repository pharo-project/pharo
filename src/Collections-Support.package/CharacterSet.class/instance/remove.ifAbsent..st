removing
remove: aCharacter ifAbsent: aBlock
	| code |
	code := aCharacter asciiValue.
	(code >= 256 or: [ (map at: code + 1) == 0 ]) ifTrue: [ ^ aBlock value ].
	map at: code + 1 put: 0.
	^ aCharacter