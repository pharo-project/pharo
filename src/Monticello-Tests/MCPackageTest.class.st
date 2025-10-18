Class {
	#name : 'MCPackageTest',
	#superclass : 'MCTestCase',
	#category : 'Monticello-Tests-Base',
	#package : 'Monticello-Tests',
	#tag : 'Base'
}

{ #category : 'private' }
MCPackageTest >> aMethodRemoved: anEvent [
	"Force cleaning of the protocol."

	(anEvent protocol name = self mockExtensionProtocol and: [ anEvent methodClass == MCSnapshotTest ]) ifTrue: [
		anEvent methodClass removeProtocolIfEmpty: self mockExtensionProtocol ]
]

{ #category : 'running' }
MCPackageTest >> tearDown [
	self mockSnapshot install.
	MCDataStream initialize. "MCMockClassG ends up in the DataStream TypeMap -- we need to reset"
	super tearDown
]

{ #category : 'tests' }
MCPackageTest >> testUnload [

	self mockPackage unload.
	self deny: (testingEnvironment hasClassNamed: #MCMockClassA).
	self deny: (MCSnapshotTest includesSelector: #mockClassExtension).
	self deny: (MCSnapshotTest hasProtocol: self mockExtensionProtocol).
	testingEnvironment at: #MCMockClassA ifPresent: [ self fail ].
	self assert: (Object subclasses
			 detect: [ :c | c name = #MCMockClassA ]
			 ifNone: [  ]) isNil
]

{ #category : 'tests' }
MCPackageTest >> testUnloadWithAdditionalTracking [
	"This is to test against another entity removing the extension protocol as well."

	| announcer |
	announcer := self class codeChangeAnnouncer.

	announcer when: MethodRemoved send: #aMethodRemoved: to: self.
	self assert: (announcer hasSubscriber: self).
	self mockPackage unload.
	self deny: (testingEnvironment hasClassNamed: #MCMockClassA).
	self deny: (MCSnapshotTest includesSelector: #mockClassExtension).
	self deny: (MCSnapshotTest hasProtocol: self mockExtensionProtocol).
	testingEnvironment at: #MCMockClassA ifPresent: [ self fail ].
	self assert: (Object subclasses
			 detect: [ :c | c name = #MCMockClassA ]
			 ifNone: [  ]) isNil.
	announcer unsubscribe: self.
	self deny: (announcer hasSubscriber: self)
]
