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
SUnitSuiteBuilder >> visitRPackage: aPackage [
	^ self
		suiteNamed: aPackage name
		withTests: (super visitRPackage: aPackage)
]

{ #category : 'visiting' }
SUnitSuiteBuilder >> visitRPackageSet: aRPackageSet [
	^ self
		suiteNamed: aRPackageSet packageName
		withTests: (super visitRPackageSet: aRPackageSet)
]

{ #category : 'visiting' }
SUnitSuiteBuilder >> visitTestCase: aTestCase [
	^ aTestCase
]

{ #category : 'visiting' }
SUnitSuiteBuilder >> visitTestSuite: aTestSuite [
	^ aTestSuite
]
