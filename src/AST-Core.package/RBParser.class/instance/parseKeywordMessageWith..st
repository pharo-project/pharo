private-parsing
parseKeywordMessageWith: node 
	| args isKeyword keywordsStartPositions selector|
	args := OrderedCollection new: 3.
	keywordsStartPositions := OrderedCollection new: 3.
	selector := String new.
	
	isKeyword := false.
	[currentToken isKeyword] whileTrue: 
			[keywordsStartPositions add: currentToken start.
			selector := selector, currentToken value.
			self step.
			args add: self parseBinaryMessage.
			isKeyword := true].
	^isKeyword
		ifTrue: 
			[self messageNodeClass 
				receiver: node
				selector: selector
				keywordsPositions: keywordsStartPositions
				arguments: args]
		ifFalse: [node]