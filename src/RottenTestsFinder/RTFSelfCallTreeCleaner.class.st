"
For a node visited, recursively clean sub-nodes that do not lead to a call to an assertion primitive.

This visitor modifies the tree it visits!
"
Class {
	#name : 'RTFSelfCallTreeCleaner',
	#superclass : 'RTFSelfCallVisitor',
	#category : 'RottenTestsFinder-SelfInterpreter',
	#package : 'RottenTestsFinder',
	#tag : 'SelfInterpreter'
}

{ #category : 'visiting' }
RTFSelfCallTreeCleaner >> visitRTFSelfCallNode: aRTFSelfCallNode [
	^ aRTFSelfCallNode
		subCalls: (aRTFSelfCallNode subCalls
						select: [ :subCallNode | subCallNode canLeadToAssertPrimitive ]
						thenCollect: [ :subCallNode | subCallNode acceptVisitor: self ]);
		yourself
]
