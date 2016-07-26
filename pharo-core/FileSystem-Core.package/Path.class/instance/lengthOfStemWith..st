private
lengthOfStemWith: aPath
	| limit index |
	limit := self size min: aPath size.
	index := 1.
	[index <= limit and: [(self at: index) = (aPath at: index)]] 
		whileTrue: [index := index + 1].
	^ index - 1