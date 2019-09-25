Class {
	#name : #MCPackageTest,
	#superclass : #MCTestCase,
	#category : #'Monticello-Tests-Base'
}

{ #category : #private }
MCPackageTest >> aMethodRemoved: anEvent [
	"Force cleaning of the protocol."

	(anEvent protocol = self mockExtensionMethodCategory and: [ anEvent methodClass == MCSnapshotTest ])
		ifTrue: [ anEvent methodClass organization removeProtocolIfEmpty: self mockExtensionMethodCategory ]
]

{ #category : #running }
MCPackageTest >> tearDown [
	self mockSnapshot install.
	MCDataStream initialize. "MCMockClassG ends up in the DataStream TypeMap -- we need to reset"
	super tearDown
]

{ #category : #tests }
MCPackageTest >> testUnload [
	| mock |
	self mockPackage unload.
	self deny: (testingEnvironment hasClassNamed: #MCMockClassA).
	self deny: (MCSnapshotTest includesSelector: #mockClassExtension).
	self deny: (MCSnapshotTest  organization protocolNamed: self mockExtensionMethodCategory) notNil.
	mock := testingEnvironment at: #MCMock.
	self assert: (mock subclasses detect: [ :c | c name = #MCMockClassA ] ifNone: [  ]) isNil
]

{ #category : #tests }
MCPackageTest >> testUnloadWithAdditionalTracking [
	"This is to test against another entity removing the extension protocol as well."

	| mock |
	SystemAnnouncer uniqueInstance when: MethodRemoved send: #aMethodRemoved: to: self.
	self assert: (SystemAnnouncer uniqueInstance hasSubscriber: self ).
	self mockPackage unload.
	self deny: (testingEnvironment hasClassNamed: #MCMockClassA).
	self deny: (MCSnapshotTest includesSelector: #mockClassExtension).
	self deny: (MCSnapshotTest  organization protocolNamed: self mockExtensionMethodCategory) notNil.
	mock := testingEnvironment at: #MCMock.
	self assert: (mock subclasses detect: [ :c | c name = #MCMockClassA ] ifNone: [  ]) isNil.
	SystemAnnouncer uniqueInstance unsubscribe: self.
	self deny: (SystemAnnouncer uniqueInstance hasSubscriber: self ).

]
