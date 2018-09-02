Class {
	#name : #ReTestBasedTestCase,
	#superclass : #ReCodeBasedTestCase,
	#instVars : [
		'validTestPackage'
	],
	#category : #'Renraku-Test'
}

{ #category : #tests }
ReTestBasedTestCase >> setUp [
	super setUp.
	
	validTestPackage := (RPackage named: #'Renraku-Nuclear-Site-Tests') register.
	
	"create tests in wrong package"
	testClass := TestCase
		subclass: 'NuclearSiteTestingClassTest'
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		package: testPackage name.
]

{ #category : #tests }
ReTestBasedTestCase >> tearDown [

	validTestPackage methods do: #removeFromSystem.
	validTestPackage definedClasses do: #removeFromSystem.
	validTestPackage unregister.
	validTestPackage := nil.	
	super tearDown
]
