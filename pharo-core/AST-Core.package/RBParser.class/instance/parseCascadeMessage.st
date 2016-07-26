private-parsing
parseCascadeMessage
	| node receiver messages semicolons |
	node := self parseKeywordMessage.
	(currentToken isSpecial and: [ currentToken value = $; ])
		ifTrue: [ (node isVariable or: [ node hasParentheses ])
				ifTrue: [ self parserError: 'cascaded message not allowed' ] ].
	(currentToken isSpecial and: [ currentToken value = $; and: [ node isMessage ] ])
		ifFalse: [ ^ node ].
	receiver := node receiver.
	messages := OrderedCollection new: 3.
	semicolons := OrderedCollection new: 3.
	messages add: node.
	[currentToken isSpecial and: [currentToken value = $;]] whileTrue: 
			[|newMessage| semicolons add: currentToken start.
			newMessage := nil.
			self step.
			self saveCommentsDuring:[
			newMessage := currentToken isIdentifier 
						ifTrue: [self parseUnaryMessageWith: receiver]
						ifFalse: 
							[currentToken isKeyword 
								ifTrue: [self parseKeywordMessageWith: receiver]
								ifFalse: 
									[| temp |
									currentToken isLiteralToken ifTrue: [self patchNegativeLiteral].
									currentToken isBinary ifFalse: [ ^ self parserError: 'Message expected'].
									temp := self parseBinaryMessageWith: receiver.
									temp == receiver ifTrue: [ ^ self parserError: 'Message expected'].
									temp]]].
					
		self addCommentsTo: newMessage.
		messages add: newMessage].
	^self cascadeNodeClass messages: messages semicolons: semicolons