tests
testFlatStructureLayout
	| flatLayout |
	flatLayout := FFITestStructureSmallFloatStructure flatStructureLayout.
	self assert: flatLayout size equals: 16.
	self assert: flatLayout alignment equals: 4.
	self assert: flatLayout fields size equals: 4.
	self assert: flatLayout fields first registerClass equals: #float.
	self assert: flatLayout fields second registerClass equals: #float.
	self assert: flatLayout fields third registerClass equals: #float.
	self assert: flatLayout fields fourth registerClass equals: #float.
