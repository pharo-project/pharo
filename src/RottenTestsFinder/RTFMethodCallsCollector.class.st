"
Collect all the CompiledMethods called in a Set.

Each CompiledMethod appears a single time in #compiledMethods even if called multiple times.
"
Class {
	#name : 'RTFMethodCallsCollector',
	#superclass : 'RTFSelfCallVisitor',
	#instVars : [
		'compiledMethods'
	],
	#category : 'RottenTestsFinder-SelfInterpreter',
	#package : 'RottenTestsFinder',
	#tag : 'SelfInterpreter'
}

{ #category : 'accessing' }
RTFMethodCallsCollector >> compiledMethods [
	^ compiledMethods
]

{ #category : 'accessing' }
RTFMethodCallsCollector >> compiledMethods: anObject [
	compiledMethods := anObject
]

{ #category : 'initialization' }
RTFMethodCallsCollector >> initialize [
	super initialize.
	self
		compiledMethods: Set new
]

{ #category : 'visiting' }
RTFMethodCallsCollector >> visitRTFSelfCallNode: aRTFSelfCallNode [
	self compiledMethods add: aRTFSelfCallNode compiledMethod.
	aRTFSelfCallNode subCalls do: [ :subCall | subCall acceptVisitor: self ]
]
