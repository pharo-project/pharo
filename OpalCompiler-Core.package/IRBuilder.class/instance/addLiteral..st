initialize
addLiteral: aLiteral
	"Add this literal at the end of the literal array if there is space left"
	aLiteral ifNil: [ ^ self ].
	ir addAdditionalLiteral: aLiteral.