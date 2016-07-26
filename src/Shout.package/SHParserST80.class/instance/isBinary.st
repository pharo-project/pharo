token testing
isBinary
	| char |
	(currentToken isNil
			or: [self isName
					or: [self isKeyword]])
		ifTrue: [^ false].
	1
		to: currentToken size
		do: [:i | 
			char := currentToken at: i.
			(self isSelectorCharacter: char)
				ifFalse: [^ false]].
	^ true