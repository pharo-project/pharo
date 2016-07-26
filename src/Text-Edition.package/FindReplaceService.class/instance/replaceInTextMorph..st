services
replaceInTextMorph: aTextMorph

	findStartIndex := self searchBackwards 
		ifTrue: [aTextMorph editor stopIndex] 
		ifFalse: [aTextMorph editor startIndex].
	(self findInTextMorph: aTextMorph)
		ifNotNil: [aTextMorph replaceSelectionWith: self replaceText]