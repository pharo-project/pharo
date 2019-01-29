"
SUnit tests for SystemNavigation on newly created environments
"
Class {
	#name : #SystemNavigationOnNewlyCreatedEnvironmentTest,
	#superclass : #SystemNavigationTest,
	#category : #'System-Support-Tests'
}

{ #category : #'building suites' }
SystemNavigationOnNewlyCreatedEnvironmentTest class >> shouldInheritSelectors [

^true
]

{ #category : #setUp }
SystemNavigationOnNewlyCreatedEnvironmentTest >> createSystemNavigationToTest [
	
	| environment nav |
	environment := SystemDictionary withOrganizer: SystemOrganizer new.
	nav := SystemNavigation new.
	nav environment: environment.
	^nav
]

{ #category : #tests }
SystemNavigationOnNewlyCreatedEnvironmentTest >> testAllExistingProtocolsFor [
	"refined from the superclass. Because in a created environment there is no existing protocols."

	| instSideProtocols classSideProtocols |
	instSideProtocols := self systemNavigationToTest allExistingProtocolsFor: true.
	classSideProtocols := self systemNavigationToTest allExistingProtocolsFor: false.
	self assertEmpty: instSideProtocols.
	self assertEmpty: classSideProtocols
]

{ #category : #tests }
SystemNavigationOnNewlyCreatedEnvironmentTest >> testAllReferencesToPool [
	| result |
	result := self systemNavigationToTest allReferencesToPool: FooSharedPool.
	self assertEmpty: result	"FooSharedPool is not in this environment"
]

{ #category : #tests }
SystemNavigationOnNewlyCreatedEnvironmentTest >> testEnsureDefaultEnvironmentNotUsed [
	| allClasses originalProtoObjectClass originalObjectClass anotherObjectClass |
	allClasses := self systemNavigationToTest allClasses.
	self assertEmpty: allClasses.
	originalProtoObjectClass := Smalltalk globals at: #ProtoObject.
	originalObjectClass := Smalltalk globals at: #Object.

	DangerousClassNotifier
		disableDuring: [ anotherObjectClass := self classFactory
				newClassNamed: #Object
				subclassOf: originalProtoObjectClass
				instanceVariableNames: ''
				classVariableNames: 'DependentsFields' ].

	self assert: (self environmentToTest at: #Object) = anotherObjectClass.
	self assert: (Smalltalk globals at: #Object) = originalObjectClass
]
