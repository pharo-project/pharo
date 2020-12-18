"
I provide a testing class and method which are destroyed later
"
Class {
	#name : #ReCodeBasedTestCase,
	#superclass : #RenrakuBaseTestCase,
	#instVars : [
		'testClass',
		'testMethod',
		'testClass2'
	],
	#category : #'Renraku-Tests'
}

{ #category : #testing }
ReCodeBasedTestCase class >> isAbstract [

	^ self = ReCodeBasedTestCase
]

{ #category : #running }
ReCodeBasedTestCase >> setUp [
	super setUp.

	testClass := self class classInstaller make: [ :aClassBuilder | 
		aClassBuilder 
			name: #RenrakuProgrammaticallyCreatedClassTestClass;
			package: testPackage name ].


	testMethod := testClass
		>> (testClass compile: 'testingMethod ^ self').

	testClass2 := self class classInstaller make: [ :aClassBuilder | 
		aClassBuilder 
			name: #RenrakuProgrammaticallyCreatedClassTestClass2;
			package: testPackage name ]
]

{ #category : #running }
ReCodeBasedTestCase >> tearDown [

	testPackage methods do: #removeFromSystem.
	testPackage definedClasses do: #removeFromSystem.
	super tearDown
]
