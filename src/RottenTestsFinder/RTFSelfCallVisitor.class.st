"
I am an abstract visitor for selfcall nodes.
"
Class {
	#name : 'RTFSelfCallVisitor',
	#superclass : 'Object',
	#category : 'RottenTestsFinder-SelfInterpreter',
	#package : 'RottenTestsFinder',
	#tag : 'SelfInterpreter'
}

{ #category : 'visiting' }
RTFSelfCallVisitor >> visitRTFSelfCallNode: aRTFSelfCallNode [
	^ self subclassResponsibility
]

{ #category : 'visiting' }
RTFSelfCallVisitor >> visitRTFSelfCallRootNode: aRTFSelfCallRootNode [
	"By default, same treatment for root and non-root node."
	^ self visitRTFSelfCallNode: aRTFSelfCallRootNode
]
