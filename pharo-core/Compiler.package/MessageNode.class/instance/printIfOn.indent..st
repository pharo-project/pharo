printing
printIfOn: aStream indent: level

	receiver ifNotNil:
		[receiver printOn: aStream indent: level + 1 precedence: precedence].
	(arguments last isJust: NodeNil) ifTrue:
		[^self printKeywords: #ifTrue: arguments: (Array with: arguments first)
					on: aStream indent: level].
	(arguments last isJust: NodeFalse) ifTrue:
		[^self printKeywords: #and: arguments: (Array with: arguments first)
					on: aStream indent: level].
	(arguments first isJust: NodeNil) ifTrue:
		[^self printKeywords: #ifFalse: arguments: (Array with: arguments last)
					on: aStream indent: level].
	(arguments first isJust: NodeTrue) ifTrue:
		[^self printKeywords: #or: arguments: (Array with: arguments last)
					on: aStream indent: level].
	self printKeywords: #ifTrue:ifFalse: arguments: arguments
					on: aStream indent: level