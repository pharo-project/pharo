tests
testAddSlot
	"self run: #testAddInstVarName"

	| tutu |
	tutu := Smalltalk globals at: #TUTU.
	tutu addSlot: #x => InstanceVariableSlot.
	self assert: tutu instVarNames = #('x').
	self assert: tutu name = #TUTU.
	tutu addSlot: #y => InstanceVariableSlot.
	self assert: tutu instVarNames = #('x' 'y')