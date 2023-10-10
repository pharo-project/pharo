"
I am a specialized `CoverageResult` for DrTests.
Like my parent:
- I represent the result of a code coverage measurement.
- My instance is produced by `DTCoverageCollector>>#basicCollectResult`.
- I contain the set of covered methods and sequence nodes.

I add the partiallyCoveredMethods attribute to store the methods that are only partially covered by tests.

I am used to display the sourceCode panel in DrTest's Coverage plugin.
"
Class {
	#name : 'DTCoverageResult',
	#superclass : 'CoverageResult',
	#instVars : [
		'partiallyCoveredMethods'
	],
	#category : 'DrTests-TestCoverage',
	#package : 'DrTests-TestCoverage'
}

{ #category : 'accessing' }
DTCoverageResult >> partiallyCoveredMethods [

	^ partiallyCoveredMethods
]

{ #category : 'accessing' }
DTCoverageResult >> partiallyCoveredMethods: anObject [

	partiallyCoveredMethods := anObject
]
