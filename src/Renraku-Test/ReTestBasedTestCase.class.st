Class {
	#name : #ReTestBasedTestCase,
	#superclass : #ReCodeBasedTestCase,
	#instVars : [
		'validTestPackage'
	],
	#category : #'Renraku-Test'
}

{ #category : #running }
ReTestBasedTestCase >> setUp [
	super setUp.
	validTestPackage := (RPackage
		named: #'Renraku-Programmatically-Created-Class-Tests') register.

	"create tests in wrong package"
	testClass := TestCase
		subclass: 'RenrakuProgrammaticallyCreatedClassTest'
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		package: testPackage name
]

{ #category : #running }
ReTestBasedTestCase >> tearDown [

	validTestPackage methods do: #removeFromSystem.
	validTestPackage definedClasses do: #removeFromSystem.
	validTestPackage unregister.
	validTestPackage := nil.	
	super tearDown
]
