private-formatting
formatSelectorAndArguments: aMessageNode firstSeparator: firstBlock restSeparator: restBlock
	| separatorBlock |
	separatorBlock := firstBlock.
	aMessageNode isUnary
		ifTrue: [ 
			self handleLineForSelector: aMessageNode selector withSeparatorBlock:
				separatorBlock.
			codeStream nextPutAll: aMessageNode selector ]
		ifFalse: [ 
			aMessageNode selectorParts
				with: aMessageNode arguments
				do: [ :selector :argument | 
					self handleLineForSelector: selector withSeparatorBlock: separatorBlock.
					separatorBlock := restBlock.
					self
						indentAround: [ 
							codeStream nextPutAll: selector.
							self handleLineForArgument: argument ] ] ]