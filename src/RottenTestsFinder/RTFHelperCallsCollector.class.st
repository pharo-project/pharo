"
Collect CompiledMethods for test helper methods called in #helperCalls inst. var.

If an helper is called multiple times, the CompiledMethod will appear multiple times.
"
Class {
	#name : 'RTFHelperCallsCollector',
	#superclass : 'RTFSelfCallVisitor',
	#instVars : [
		'helperCalls'
	],
	#category : 'RottenTestsFinder-SelfInterpreter',
	#package : 'RottenTestsFinder',
	#tag : 'SelfInterpreter'
}

{ #category : 'accessing' }
RTFHelperCallsCollector >> helperCalls [
	^ helperCalls
]

{ #category : 'accessing' }
RTFHelperCallsCollector >> helperCalls: anObject [
	helperCalls := anObject
]

{ #category : 'initialization' }
RTFHelperCallsCollector >> initialize [
	super initialize.
	self
		helperCalls: OrderedCollection new
]

{ #category : 'visiting' }
RTFHelperCallsCollector >> visitRTFSelfCallNode: aRTFSelfCallNode [
	(RottenTestsFinder assertionSelectors includes: aRTFSelfCallNode compiledMethod selector)
		ifFalse: [ self helperCalls add: aRTFSelfCallNode compiledMethod ].
	aRTFSelfCallNode subCalls do: [ :subCall | subCall acceptVisitor: self ]
]

{ #category : 'visiting' }
RTFHelperCallsCollector >> visitRTFSelfCallRootNode: aRTFSelfCallRootNode [
	aRTFSelfCallRootNode subCalls do: [ :subCall | subCall acceptVisitor: self ]
]
