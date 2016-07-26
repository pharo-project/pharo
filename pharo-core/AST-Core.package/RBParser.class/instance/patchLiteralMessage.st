private
patchLiteralMessage
	currentToken value == true 
		ifTrue: 
			[^currentToken := RBIdentifierToken value: 'true' start: currentToken start].
	currentToken value == false 
		ifTrue: 
			[^currentToken := RBIdentifierToken value: 'false' start: currentToken start].
	currentToken value isNil 
		ifTrue: 
			[^currentToken := RBIdentifierToken value: 'nil' start: currentToken start]