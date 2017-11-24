"
Answer the set of temporary variables that are read before they are written in the visited parse tree.  Used by the compiler to detect those block-local temporaries of blocks in optimized loops that require nilling to prevent a value from a previous iteration persisting into a subsequent one.
"
Class {
	#name : #OptimizedBlockLocalTempReadBeforeWrittenVisitor,
	#superclass : #ParseNodeVisitor,
	#instVars : [
		'inOptimizedBlock',
		'readBeforeWritten',
		'written'
	],
	#category : #'Compiler-Support'
}

{ #category : #initialization }
OptimizedBlockLocalTempReadBeforeWrittenVisitor >> initialize [
	inOptimizedBlock := false
]

{ #category : #accessing }
OptimizedBlockLocalTempReadBeforeWrittenVisitor >> readBeforeWritten [
	^readBeforeWritten ifNil: [IdentitySet new]
]

{ #category : #visiting }
OptimizedBlockLocalTempReadBeforeWrittenVisitor >> visitAssignmentNode: anAssignmentNode [
	anAssignmentNode value accept: self.
	anAssignmentNode variable isTemp
		ifTrue:
			[written ifNil: [written := IdentitySet new].
			 written add: anAssignmentNode variable]
		ifFalse:
			[anAssignmentNode variable accept: self]
]

{ #category : #visiting }
OptimizedBlockLocalTempReadBeforeWrittenVisitor >> visitBlockNode: aBlockNode [
	| savedWritten |
	"If we're in the optimized block in one side of an optimized ifTrue:ifFalse: et al
	 leave it to the enclosing visitMessageNode: activation to handle merging written."
	inOptimizedBlock ifTrue:
		[^super visitBlockNode: aBlockNode].
	"If we're not then don't update written because without evaluating the guard(s)
	 we can't tell if the block is evaluated or not, and we must avoid false positives."
	savedWritten := written copy.
	super visitBlockNode: aBlockNode.
	written := savedWritten
]

{ #category : #visiting }
OptimizedBlockLocalTempReadBeforeWrittenVisitor >> visitMessageNode: aMessageNode [
	| savedWritten writtenPostFirstArm |
	(aMessageNode isOptimized
	 and: [#(ifTrue:ifFalse: ifFalse:ifTrue: ifNil:ifNotNil: ifNotNil:ifNil:) includes: aMessageNode selector key]) ifFalse:
		[^super visitMessageNode: aMessageNode].
	aMessageNode receiver accept: self.
	aMessageNode selector accept: self.
	savedWritten := written copy.
	aMessageNode argumentsInEvaluationOrder
		do: [:argument|
			argument isBlockNode
				ifTrue: [| savedIOB |
					savedIOB := inOptimizedBlock.
					inOptimizedBlock := true.
					[argument accept: self]
						ensure: [inOptimizedBlock := savedIOB]]
				ifFalse: [argument accept: self]]
		separatedBy:
			[writtenPostFirstArm := written.
			 written := savedWritten].
	(written notNil
	 and: [writtenPostFirstArm notNil]) ifTrue:
		[written := written intersection: writtenPostFirstArm]
]

{ #category : #visiting }
OptimizedBlockLocalTempReadBeforeWrittenVisitor >> visitTempVariableNode: aTempVariableNode [
	(aTempVariableNode isArg
	 or: [written notNil
		and: [written includes: aTempVariableNode]]) ifTrue:
		[^self].
	readBeforeWritten ifNil:
		[readBeforeWritten := IdentitySet new].
	readBeforeWritten add: aTempVariableNode
]
