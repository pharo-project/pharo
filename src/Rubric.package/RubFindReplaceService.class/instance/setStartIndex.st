private
setStartIndex
	findStartIndex
		ifNil: [ findStartIndex := self searchBackwards
				ifTrue: [ self textArea editor stopIndex ]
				ifFalse: [ self textArea editor startIndex ] ]