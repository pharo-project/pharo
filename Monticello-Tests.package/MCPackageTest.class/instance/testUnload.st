tests
testUnload
	| mock |
	self mockPackage unload.
	self deny: (Smalltalk hasClassNamed: #MCMockClassA).
	self deny: (MCSnapshotTest includesSelector: #mockClassExtension).
	self deny: (MCSnapshotTest  organization protocolNamed: self mockExtensionMethodCategory) notNil.
	mock := Smalltalk globals at: #MCMock.
	self assert: (mock subclasses detect: [ :c | c name = #MCMockClassA ] ifNone: [  ]) isNil