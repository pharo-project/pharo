"
Collect CompiledMethods for assertion primitives called in #primitiveCalls inst. var.

If a primitive is called multiple times, the CompiledMethod will appear multiple times.
"
Class {
	#name : 'RTFPrimitiveCallsCollector',
	#superclass : 'RTFSelfCallVisitor',
	#instVars : [
		'primitiveCalls'
	],
	#category : 'RottenTestsFinder-SelfInterpreter',
	#package : 'RottenTestsFinder',
	#tag : 'SelfInterpreter'
}

{ #category : 'initialization' }
RTFPrimitiveCallsCollector >> initialize [
	super initialize.
	self
		primitiveCalls: OrderedCollection new
]

{ #category : 'accessing' }
RTFPrimitiveCallsCollector >> primitiveCalls [
	^ primitiveCalls
]

{ #category : 'accessing' }
RTFPrimitiveCallsCollector >> primitiveCalls: anObject [
	primitiveCalls := anObject
]

{ #category : 'visiting' }
RTFPrimitiveCallsCollector >> visitRTFSelfCallNode: aRTFSelfCallNode [
	(RottenTestsFinder assertionSelectors includes: aRTFSelfCallNode compiledMethod selector)
		ifTrue: [ self primitiveCalls add: aRTFSelfCallNode compiledMethod ].
	aRTFSelfCallNode subCalls do: [ :subCall | subCall acceptVisitor: self ]
]
