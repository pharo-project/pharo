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

{ #category : #'setUp-tearDown' }
SystemNavigationOnNewlyCreatedEnvironmentTest >> createSystemNavigationToTest [
	
	| environment nav |
	environment := SystemDictionary withOrganizer: SystemOrganizer new.
	nav := SystemNavigation new.
	nav environment: environment.
	^nav
]

{ #category : #testing }
SystemNavigationOnNewlyCreatedEnvironmentTest >> testAllReferencesToPool [
	| result |
	
	result := self systemNavigationToTest allReferencesToPool: FooSharedPool.
	self assert: result isEmpty. "FooSharedPool is not in this environment"
]

{ #category : #testing }
SystemNavigationOnNewlyCreatedEnvironmentTest >> testEnsureDefaultEnvironmentNotUsed [
	
	| allClasses originalProtoObjectClass originalObjectClass anotherObjectClass |
	allClasses := self systemNavigationToTest allClasses.
	self assert: allClasses isEmpty.
	originalProtoObjectClass := Smalltalk globals at: #ProtoObject.		
	originalObjectClass := Smalltalk globals at: #Object.
	
	DangerousClassNotifier disableDuring: [
		anotherObjectClass := self classFactory newClassNamed: #Object subclassOf: originalProtoObjectClass instanceVariableNames: '' classVariableNames:  'DependentsFields'].
	
	self assert: ((self environmentToTest at: #Object) = anotherObjectClass).
	self assert: ((Smalltalk globals at: #Object) = originalObjectClass).
	
]
