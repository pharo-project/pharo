"
This visitor returns true if the node visited leads to an assert primitive call.

Else it returns false.
"
Class {
	#name : 'RTFLeadsToAssertPrimitiveCallChecker',
	#superclass : 'RTFSelfCallVisitor',
	#category : 'RottenTestsFinder-SelfInterpreter',
	#package : 'RottenTestsFinder',
	#tag : 'SelfInterpreter'
}

{ #category : 'visiting' }
RTFLeadsToAssertPrimitiveCallChecker >> visitRTFSelfCallNode: aRTFSelfCallNode [
	(aRTFSelfCallNode subCalls isEmpty and: [ RottenTestsFinder assertionCompiledMethods includes: aRTFSelfCallNode compiledMethod ])
		ifTrue: [ ^ true ].

	^ aRTFSelfCallNode subCalls anySatisfy: [ :subCallNode |
		self visitRTFSelfCallNode: subCallNode ]
]
