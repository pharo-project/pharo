tests
testAddInstVarName
	"self run: #testAddInstVarName"

	| tutu |
	tutu := Smalltalk globals at: #TUTU.
	tutu addInstVarNamed: 'x'.
	self assert: tutu instVarNames = #('x').
	tutu addInstVarNamed: 'y'.
	self assert: tutu instVarNames = #('x' 'y')