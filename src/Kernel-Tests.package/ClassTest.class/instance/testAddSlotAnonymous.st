tests
testAddSlotAnonymous
	"self run: #testAddInstVarName"

	| tutu |
	tutu := Object newAnonymousSubclass.
	self assert: tutu getName isNil.
	tutu := tutu addSlot: #x => InstanceVariableSlot.
	self assert: tutu instVarNames = #('x').
	self assert: tutu getName isNil.
	tutu := tutu addSlot: #y => InstanceVariableSlot.
	self assert: tutu getName isNil.
	self assert: tutu instVarNames = #('x' 'y')