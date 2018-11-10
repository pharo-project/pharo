"
I am a removal in a tree. I contain the definition I'm removing. I know what is the icon for removal and how to calculate a conflict with other operations.
"
Class {
	#name : #IceRemoval,
	#superclass : #IceOperation,
	#category : #'Iceberg-Changes'
}

{ #category : #visiting }
IceRemoval >> accept: aVisitor [
	
	^ aVisitor visitRemoval: self
]

{ #category : #operations }
IceRemoval >> inverse [
	
	^ IceAddition definition: definition
]

{ #category : #testing }
IceRemoval >> isRemoval [
	
	^ true
]

{ #category : #accessing }
IceRemoval >> leftContents [
	
	^ ''
]

{ #category : #merging }
IceRemoval >> mergeWithLeftAddition: anIceAddition [ 
	
	"This should not happen. If I represent a removal of a node, that means that the original definition existed on the base version.
	Thus, the right version can never ADD an already existing version. It can remove it too or modify it."
	self shouldNotImplement
]

{ #category : #merging }
IceRemoval >> mergeWithLeftModification: anIceModification [ 
	
	^ IceConflictingOperation
		left: anIceModification
		right: self
]

{ #category : #merging }
IceRemoval >> mergeWithLeftNoOperation: anIceNoModification [ 
	
	"We assume that if the IceNoModification is in the tree this means its children have modifications.
	So a removal of a tree with modifications should be a conflict.
	
	This should maybe be enhanced: if my children are removals maybe this should not be a conflict, but this is the simplest solution right now."
	^ IceConflictingOperation left: anIceNoModification right: self
]

{ #category : #merging }
IceRemoval >> mergeWithLeftRemoval: anIceRemoval [ 
	
	^ IceNonConflictingOperation operation: anIceRemoval
]

{ #category : #merging }
IceRemoval >> mergeWithOperation: anOperation [
	
	^ anOperation mergeWithLeftRemoval: self
]
