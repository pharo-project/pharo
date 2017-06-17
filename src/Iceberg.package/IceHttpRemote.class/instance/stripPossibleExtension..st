private
stripPossibleExtension: aString
	^ (aString endsWith: '.git')
		ifTrue: [ aString allButLast: 4 ]
		ifFalse: [ aString ]