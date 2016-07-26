testing
testChangingShapeDoesNotPutNilInMethodsLastLiteralKey
	"Test that when the shape of a class changes, the key of the last literal of the methods is not nil"
	| tutu |
	tutu := Smalltalk globals at: #TUTU.
	tutu compile: 'foo'.
	self deny: (tutu >> #foo) allLiterals last key isNil.
	tutu addInstVarNamed: 'x'.
	self deny: (tutu >> #foo) allLiterals last key isNil.