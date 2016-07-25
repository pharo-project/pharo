private
previousNonBlank: position
	"decrement position up until any non-blank character found, or end of line"
	| string index |
	position <= 1 ifTrue: [ ^ 1 ].
	
	string := self string.
	index := position.
	index := index -1.
	(string at: index) isSeparator ifFalse: [ ^ index ]. 

	[ index > 1 and: [(string at: index-1) isSeparator and: [(string at:index-1) ~= Character cr]]]
		whileTrue: [index := index - 1].
	^ index