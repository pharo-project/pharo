services
find
	| where  |
	findStartIndex 
		ifNil: [findStartIndex := self searchBackwards 
			ifTrue: [self textArea editor stopIndex]
			ifFalse: [self textArea  editor startIndex]].
	findStartIndex > 0 
		ifTrue: [where := self textArea 
			findAndSelect: self findRegex
			startingAt: findStartIndex
			searchBackwards: self searchBackwards].
	(where isNil and: [self wrapAround])
		ifTrue: [ | idx |
			idx := self searchBackwards ifTrue: [self textArea  editor string size] ifFalse: [1].
			where := self textArea 
				findAndSelect: self findRegex
				startingAt: idx
				searchBackwards: self searchBackwards].
	where ifNil: [self textArea  flash].
	^ where
