"
I  am a visitor that builds a TestSuite.
"
Class {
	#name : #SUnitSuiteBuilder,
	#superclass : #SUnitVisitor,
	#category : #'SUnit-Visitor'
}

{ #category : #visiting }
SUnitSuiteBuilder >> suiteNamed: aString withTests: tests [
	^ (TestSuite named: aString)
		addTests: tests;
		yourself
]

{ #category : #visiting }
SUnitSuiteBuilder >> visitRPackage: aRPackage [
	^ self
		suiteNamed: aRPackage name
		withTests: (super visitRPackage: aRPackage)
]

{ #category : #visiting }
SUnitSuiteBuilder >> visitRPackageSet: aRPackageSet [
	^ self
		suiteNamed: aRPackageSet packageName
		withTests: (super visitRPackageSet: aRPackageSet)
]

{ #category : #visiting }
SUnitSuiteBuilder >> visitTestCase: aTestCase [
	^ aTestCase
]

{ #category : #visiting }
SUnitSuiteBuilder >> visitTestSuite: aTestSuite [
	^ aTestSuite
]
