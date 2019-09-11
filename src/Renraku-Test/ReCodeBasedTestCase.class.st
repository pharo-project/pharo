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
	#category : #'Renraku-Test'
}

{ #category : #running }
ReCodeBasedTestCase >> setUp [
	super setUp.
	testClass := Object
		subclass: 'RenrakuProgrammaticallyCreatedClassTestClass'
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		package: testPackage name.
	testMethod := testClass
		>> (testClass compile: 'testingMethod ^ self').
	testClass2 := Object
		subclass: 'RenrakuProgrammaticallyCreatedClassTestClass2'
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		package: testPackage name
]

{ #category : #running }
ReCodeBasedTestCase >> tearDown [

	testPackage methods do: #removeFromSystem.
	testPackage definedClasses do: #removeFromSystem.
	super tearDown
]
