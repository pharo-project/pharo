"
I  am a visitor that builds a TestSuite.
"
Class {
	#name : 'SUnitSuiteBuilder',
	#superclass : 'SUnitVisitor',
	#category : 'SUnit-Visitor',
	#package : 'SUnit-Visitor'
}

{ #category : 'visiting' }
SUnitSuiteBuilder >> suiteNamed: aString withTests: tests [
	^ (TestSuite named: aString)
		addTests: tests;
		yourself
]

{ #category : 'visiting' }
SUnitSuiteBuilder >> visitPackage: aPackage [

	^ self suiteNamed: aPackage name withTests: (super visitPackage: aPackage)
]

{ #category : 'visiting' }
SUnitSuiteBuilder >> visitTestCase: aTestCase [
	^ aTestCase
]

{ #category : 'visiting' }
SUnitSuiteBuilder >> visitTestSuite: aTestSuite [
	^ aTestSuite
]
