"
Find rotten helper methods in the self call tree.

#rottenHelpers is a Set of CompiledMethods being rotten.
"
Class {
	#name : 'RTFRottenHelpersFinder',
	#superclass : 'RTFSelfCallVisitor',
	#instVars : [
		'rottenHelpers'
	],
	#category : 'RottenTestsFinder-SelfInterpreter',
	#package : 'RottenTestsFinder',
	#tag : 'SelfInterpreter'
}

{ #category : 'adding' }
RTFRottenHelpersFinder >> addRottenHelper: aRottenHelper [
	self rottenHelpers add: aRottenHelper
]

{ #category : 'initialization' }
RTFRottenHelpersFinder >> initialize [
	super initialize.
	self rottenHelpers: Set new
]

{ #category : 'accessing' }
RTFRottenHelpersFinder >> rottenHelpers [
	^ rottenHelpers
]

{ #category : 'accessing' }
RTFRottenHelpersFinder >> rottenHelpers: anObject [
	rottenHelpers := anObject
]

{ #category : 'visiting' }
RTFRottenHelpersFinder >> visitRTFSelfCallNode: aRTFSelfCallNode [

	(aRTFSelfCallNode subCalls allSatisfy: #hasBeenExecuted)
		ifTrue: [
			aRTFSelfCallNode subCalls do: [ :selfSubCall |
		 		selfSubCall acceptVisitor: self ].
			^ self ].

	self addRottenHelper: ((RottenTestHelper compiledMethod: aRTFSelfCallNode compiledMethod)
										callSites: (aRTFSelfCallNode subCalls reject: #hasBeenExecuted thenCollect: #astNode);
										yourself).
	aRTFSelfCallNode subCalls
		select: #hasBeenExecuted
		thenDo: [ :selfSubCall | selfSubCall acceptVisitor: self ]
]
