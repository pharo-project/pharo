Class {
	#name : #NewMethodFinderRBNodeVisitor,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'methodFinder',
		'isSafe',
		'class'
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

{ #category : #accessing }
NewMethodFinderRBNodeVisitor >> isSafe [

	^isSafe
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitGlobalNode: aGlobalNode [
	isSafe := false.
	^ super visitGlobalNode: aGlobalNode.
]

{ #category : #visiting }
NewMethodFinderRBNodeVisitor >> visitMessageNode: aMessageNode [
	isSafe := (aMessageNode selector implementors) allSatisfy: [ :s |
		|v|
		Transcript show: s ast; cr.
		v := NewMethodFinderRBNodeVisitor new visitNode: (s ast body).
		v isSafe .
	].
	isSafe ifTrue: [super visitMessageNode: aMessageNode].
]
