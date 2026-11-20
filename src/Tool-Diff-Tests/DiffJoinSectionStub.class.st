"
I stand for a join of a DiffJoinMorph that covers every point, so that any click on the morph hits me.
"
Class {
	#name : 'DiffJoinSectionStub',
	#superclass : 'Object',
	#category : 'Tool-Diff-Tests',
	#package : 'Tool-Diff-Tests'
}

{ #category : 'actions' }
DiffJoinSectionStub >> clicked [

	^ true
]

{ #category : 'testing' }
DiffJoinSectionStub >> containsPoint: aPoint [

	^ true
]
