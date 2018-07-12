Class {
	#name : #NewMethodFinderRBNodeVisitor,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'methodFinder',
		'isSafe'
	],
	#classInstVars : [
		'isSafe',
		'methodFinder'
	],
	#category : #NewMethodFinder
}

{ #category : #initialization }
NewMethodFinderRBNodeVisitor >> initialize [

	super initialize.
	isSafe := true.
	methodFinder := NewMethodFinder new.
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitGlobalNode: aGlobalNode [
	isSafe := false.
	^ super visitGlobalNode: aGlobalNode.
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitMessageNode: aMessageNode [
	"isSafe := methodFinder isSafe: aMessageNode selector."
	isSafe ifTrue: [self visitMessageNode: aMessageNode].
]
