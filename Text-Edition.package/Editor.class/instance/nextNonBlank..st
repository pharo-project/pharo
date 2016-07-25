private
nextNonBlank: position

"if current position is not separator, advance by 1, otherwise advance up until non-separator char found or new line"
	
	| string index |
	string := self string.
	index := position.
	index >= string size ifTrue: [ ^ string size+1 ].
	
	(string at: index) isSeparator ifFalse: [ ^ index + 1 ].
	index := index + 1.
	[ index <= string size and: [(string at: index) isSeparator and: [(string at:index) ~= Character cr]]]
		whileTrue: [index := index + 1].
	^ index