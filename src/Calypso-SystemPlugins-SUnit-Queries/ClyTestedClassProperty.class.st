"
I mark classes which has covering test case. 
I include reference to coverint test class.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	coveringTestCase:		<TestCase>
"
Class {
	#name : #ClyTestedClassProperty,
	#superclass : #ClyProperty,
	#instVars : [
		'coveringTestCase'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Queries'
}

{ #category : #accessing }
ClyTestedClassProperty class >> coveringTestCase: aTestCase [
	^self new 
		coveringTestCase: aTestCase
]

{ #category : #accessing }
ClyTestedClassProperty >> coveringTestCase [
	^ coveringTestCase
]

{ #category : #accessing }
ClyTestedClassProperty >> coveringTestCase: anObject [
	coveringTestCase := anObject
]
