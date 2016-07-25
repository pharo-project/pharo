tests - copying with or without
testForceToPaddingWith

	| result element |
	element := self nonEmpty at: self indexInNonEmpty .
	result := self nonEmpty forceTo: (self nonEmpty size+2) paddingWith: ( element ).
	
	"verify content of 'result' : "
	1 to: self nonEmpty  size do:
		[:i | self assert: ( self nonEmpty at: i ) = ( result at:(i) ). ].
	
	(result size - 1) to: result size do:
		[:i | self assert: ( result at:i ) = ( element ) ].

	"verify size of 'result' :"
	self assert: result size = (self nonEmpty size + 2).