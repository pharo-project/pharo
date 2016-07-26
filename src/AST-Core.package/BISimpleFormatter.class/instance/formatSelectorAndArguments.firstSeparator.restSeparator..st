private-formatting
formatSelectorAndArguments: aMessageNode firstSeparator: firstBlock restSeparator: restBlock
	aMessageNode isUnary
		ifTrue: [ 
			self space.
			codeStream nextPutAll: aMessageNode selector ]
		ifFalse: [ 
			aMessageNode selectorParts
				with: aMessageNode arguments
				do: [ :selector :argument | 
					self space.
					self
						indentAround: [ 
							codeStream nextPutAll: selector.
							self handleLineForArgument: argument ] ] ]