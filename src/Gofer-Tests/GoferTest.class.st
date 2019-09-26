"
Abstract superclass for Gofer tests
"
Class {
	#name : #GoferTest,
	#superclass : #AbstractEnvironmentTestCase,
	#instVars : [
		'gofer'
	],
	#category : #'Gofer-Tests-Tests'
}

{ #category : #testing }
GoferTest class >> isAbstract [
	^ self name = #GoferTest
]

{ #category : #testing }
GoferTest class >> isUnitTest [
	^false
]

{ #category : #accessing }
GoferTest class >> packageNamesUnderTest [
	^ #('Gofer-Core')
]

{ #category : #accessing }
GoferTest class >> resources [
	^ Array with: GoferResource
]

{ #category : #accessing }
GoferTest >> monticelloRepository [
	^ GoferResource current monticelloRepository
]

{ #category : #running }
GoferTest >> setUp [
	super setUp.
	gofer := Gofer new.
	gofer disablePackageCache
]

{ #category : #accessing }
GoferTest >> versionReferences [
	^ GoferResource current versionReferences
]
