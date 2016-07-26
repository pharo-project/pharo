tests
testUnloadWithAdditionalTracking
	"This is to test against another entity removing the extension protocol as well."

	| mock |
	SystemAnnouncer uniqueInstance when: MethodRemoved send: #aMethodRemoved: to: self.
	self assert: (SystemAnnouncer uniqueInstance hasSubscriber: self ).
	self mockPackage unload.
	self deny: (Smalltalk hasClassNamed: #MCMockClassA).
	self deny: (MCSnapshotTest includesSelector: #mockClassExtension).
	self deny: (MCSnapshotTest  organization protocolNamed: self mockExtensionMethodCategory) notNil.
	mock := Smalltalk globals at: #MCMock.
	self assert: (mock subclasses detect: [ :c | c name = #MCMockClassA ] ifNone: [  ]) isNil.
	SystemAnnouncer uniqueInstance unsubscribe: self.
	self deny: (SystemAnnouncer uniqueInstance hasSubscriber: self ).
