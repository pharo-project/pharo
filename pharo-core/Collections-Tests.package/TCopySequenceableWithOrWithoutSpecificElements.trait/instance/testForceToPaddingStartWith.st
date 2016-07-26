tests - copying with or without
testForceToPaddingStartWith

	| result element |
	element := self nonEmpty at: self indexInNonEmpty .
	result := self nonEmpty forceTo: (self nonEmpty size+2) paddingStartWith: ( element ).
	
	"verify content of 'result' : "
	1 to: 2   do:
		[:i | self assert: ( element ) = ( result at:(i) ) ].
	
	3 to: result size do:
		[:i | self assert: ( result at:i ) = ( self nonEmpty at:(i-2) ) ].

	"verify size of 'result' :"
	self assert: result size = (self nonEmpty size + 2).