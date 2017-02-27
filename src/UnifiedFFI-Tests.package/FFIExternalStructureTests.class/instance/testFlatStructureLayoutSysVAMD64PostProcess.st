tests
testFlatStructureLayoutSysVAMD64PostProcess
	| flatLayout |
	flatLayout := FFITestStructureSmallFloatStructure flatStructureLayout sysVAMD64PostProcess.
	self assert: flatLayout size equals: 16.
	self assert: flatLayout alignment equals: 4.
	self assert: flatLayout fields size equals: 2.
	self assert: flatLayout fields first registerClass equals: #float.
	self assert: flatLayout fields second registerClass equals: #float.
	self assert: flatLayout integerRegisterCount equals: 0.
	self assert: flatLayout floatRegisterCount equals: 2.