services
findNext
	| where  |
	findStartIndex 
		ifNil: [findStartIndex := self textArea editor startIndex].
	findStartIndex > 0 
		ifTrue: [where := self textArea findNextString: self findString startingAt: findStartIndex].
	(where isNil and: [self wrapAround])
		ifTrue: [where := self textArea findNextString: self findString asString startingAt: 1].
	^ where
