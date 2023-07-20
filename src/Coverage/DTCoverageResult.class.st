"
This inherits from `CoverageResult` and like the parent class:

•	This class represents the result of a code coverage measure.
•	This instance is produced by `DTCoverageCollector>>#basicCollectResult`.
•	It contains the set of covered methods and sequence nodes.


In this subclass, I add the partillyCoveredMethods attribute. It stores the method that has some tests covered but not all the source code of the method.

It helps to show the panel of sourceCode in the Coverage Pluguin of DrTestt
"
Class {
	#name : #DTCoverageResult,
	#superclass : #CoverageResult,
	#instVars : [
		'partiallyCoveredMethods'
	],
	#category : #'Coverage-Base'
}

{ #category : #accessing }
DTCoverageResult >> partiallyCoveredMethods [

	^ partiallyCoveredMethods
]

{ #category : #accessing }
DTCoverageResult >> partiallyCoveredMethods: anObject [

	partiallyCoveredMethods := anObject
]
