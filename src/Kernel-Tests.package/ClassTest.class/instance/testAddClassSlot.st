tests
testAddClassSlot

	| tutu |
	tutu := Smalltalk globals at: #TUTU.
	tutu addClassSlot: #'X' => InstanceVariableSlot.
	self assert: tutu class instVarNames = #(#'X').
	tutu addClassSlot: #'Y' => InstanceVariableSlot.
	self assert: tutu class instVarNames = #(#'X' #'Y')