services
replace
	findStartIndex := self searchBackwards 
		ifTrue: [self textArea editor stopIndex] 
		ifFalse: [self textArea editor startIndex].
	(self find)
		ifNotNil: [self textArea replaceSelectionWith: self replaceText]