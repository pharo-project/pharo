parsing
parse
	| result |
	result := self whitespace; parseValue.
	stream atEnd
		ifFalse: [ self error: 'end of input expected' ].
	^ result