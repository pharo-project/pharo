Class {
	#name : #MyClassCovered100Test,
	#superclass : #TestCase,
	#instVars : [
		'aMyClassCovered100'
	],
	#category : #'TestRunner-Tests-Utilities-Test'
}

{ #category : #initialization }
MyClassCovered100Test >> setUp [
	super setUp.
	aMyClassCovered100 := MyClassCovered100 new.
	
]

{ #category : #tests }
MyClassCovered100Test >> testCoveredMethod [

	self skipOnPharoCITestingEnvironment.
	self assert: aMyClassCovered100 coveredMethod
]

{ #category : #tests }
MyClassCovered100Test >> testOtherCoveredMethod [

	self skipOnPharoCITestingEnvironment.
	self assert: aMyClassCovered100 anotherCoveredMethod
]
