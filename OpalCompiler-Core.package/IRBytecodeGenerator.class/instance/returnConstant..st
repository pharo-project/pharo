instructions
returnConstant: constant
	bytes ifEmpty: [ lastSpecialReturn := Message selector: #returnConstant: argument: constant ].
	(#(nil true false) includes: constant) ifTrue: 
		[ ^ encoder genReturnSpecialLiteral:constant ].
	self pushLiteral: constant.
	self returnTop