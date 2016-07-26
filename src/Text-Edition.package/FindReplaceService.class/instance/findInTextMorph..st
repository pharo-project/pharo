services
findInTextMorph: aTextMorph
	| where  |
	findStartIndex 
		ifNil: [findStartIndex := self searchBackwards 
			ifTrue: [aTextMorph editor stopIndex]
			ifFalse: [aTextMorph editor startIndex]].
	findStartIndex > 0 
		ifTrue: [where := aTextMorph
			findAndSelect: self findRegex
			startingAt: findStartIndex
			searchBackwards: self searchBackwards].
	(where isNil and: [self wrapAround])
		ifTrue: [ | idx |
			idx := self searchBackwards ifTrue: [aTextMorph editor string size] ifFalse: [1].
			where := aTextMorph
				findAndSelect: self findRegex
				startingAt: idx
				searchBackwards: self searchBackwards].
	where ifNil: [aTextMorph flash].
	^ where
