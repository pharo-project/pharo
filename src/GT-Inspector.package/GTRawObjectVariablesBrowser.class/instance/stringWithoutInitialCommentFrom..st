private building
stringWithoutInitialCommentFrom: aString
	^ (aString trimLeft isEmpty not and: [ aString trimLeft first = $" ])
		ifFalse: [ aString trimRight ]
		ifTrue: [|startIndex doubleQuoteCounter|
			startIndex := aString findAnySubstring: '"' startingAt: (aString indexOf: $") + 1.
			doubleQuoteCounter := 1.
			"we need to iterate over the whole string queuing quotes to support nested double quotes"
			[ startIndex <= aString size
				and: [ doubleQuoteCounter % 2 = 1
					or: [ (aString at: startIndex) = $"
						or: [ (aString at: startIndex) isSeparator ] ] ] ]
			whileTrue: [ 
				(aString at: startIndex) = $" ifTrue: [ 
					doubleQuoteCounter := doubleQuoteCounter + 1 ].
				startIndex := startIndex + 1 ].
	
			(startIndex >= (aString size + 1)) ifTrue: [ "No content if a comment cannot be identified. " ^ '' ].
			(aString
				copyFrom: startIndex
				to: aString size) trimBoth ]