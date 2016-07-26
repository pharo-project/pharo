services
findNextSubstring: aSubstring inTextMorph: aTextMorph
	| where  |
	findStartIndex 
		ifNil: [findStartIndex := aTextMorph editor startIndex].
	findStartIndex > 0 
		ifTrue: [where := aTextMorph findNextString: aSubstring asString startingAt: findStartIndex].
	(where isNil and: [self wrapAround])
		ifTrue: [where := aTextMorph findNextString: aSubstring asString startingAt: 1].
	^ where
