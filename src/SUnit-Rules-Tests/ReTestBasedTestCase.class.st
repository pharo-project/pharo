Class {
	#name : 'ReTestBasedTestCase',
	#superclass : 'ReCodeBasedTestCase',
	#instVars : [
		'validTestPackage'
	],
	#category : 'SUnit-Rules-Tests-Base',
	#package : 'SUnit-Rules-Tests',
	#tag : 'Base'
}

{ #category : 'testing' }
ReTestBasedTestCase class >> isAbstract [

	^self == ReTestBasedTestCase 
]

{ #category : 'running' }
ReTestBasedTestCase >> setUp [

	super setUp.
	validTestPackage := self packageOrganizer ensurePackage: #'Renraku-Programmatically-Created-Class-Tests'.

	"create tests in wrong package"
	testClass := self class classInstaller make: [ :builder |
		             builder
			             superclass: TestCase;
			             name: #RenrakuProgrammaticallyCreatedClassTest;
			             package: testPackage name ]
]

{ #category : 'running' }
ReTestBasedTestCase >> tearDown [

	validTestPackage removeFromSystem.
	super tearDown
]
