tests
testFlatStructureLayoutSysVAMD64PostProcess2
	| flatLayout |
	flatLayout := FFITestStructureSmallIntFloatStructure flatStructureLayout sysVAMD64PostProcess.
	self assert: flatLayout size equals: 16.
	self assert: flatLayout alignment equals: 4.
	self assert: flatLayout fields size equals: 2.
	self assert: flatLayout fields first registerClass equals: #integer.
	self assert: flatLayout fields second registerClass equals: #float.
	self assert: flatLayout integerRegisterCount equals: 1.
	self assert: flatLayout floatRegisterCount equals: 1.
	