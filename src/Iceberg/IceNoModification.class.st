"
I am a no modification in a tree. I am useful to represent a container (that did not change) containing elements that did change. I contain the definition I represent to be able to show it. I delegate the icon to my definition and I know how to calculate a conflict with other operations.
"
Class {
	#name : #IceNoModification,
	#superclass : #IceOperation,
	#category : #'Iceberg-Changes'
}

{ #category : #visiting }
IceNoModification >> accept: aVisitor [
	
	^ aVisitor visitNoModification: self
]

{ #category : #testing }
IceNoModification >> hasChanges [
	
	^ false
]

{ #category : #operations }
IceNoModification >> inverse [
	
	^ self
]

{ #category : #testing }
IceNoModification >> isConflict [
	^ false
]

{ #category : #accessing }
IceNoModification >> leftContents [
	
	^ definition contents
]

{ #category : #merging }
IceNoModification >> mergeWithLeftAddition: anIceAddition [ 
	
	"This should not happen. If I represent a no modification of a node, that means that the original definition existed on the base version.
	Thus, the right version can never ADD an already existing version. It can remove it or modify it."
	self shouldNotImplement
]

{ #category : #merging }
IceNoModification >> mergeWithLeftModification: anIceModification [ 
	
	^ IceNonConflictingOperation operation: anIceModification
]

{ #category : #merging }
IceNoModification >> mergeWithLeftNoOperation: anIceNoModification [ 
	
	"There was no modifications on both sides, I can just return the left side"
	^ IceNonConflictingOperation operation: anIceNoModification
]

{ #category : #merging }
IceNoModification >> mergeWithLeftRemoval: anIceRemoval [
	
	"We assume that if I'm in the operation tree this means my children have modifications.
	So a removal of a tree with modifications should be a conflict.
	
	This should maybe be enhanced: if my children are removals maybe this should not be a conflict, but this is the simplest solution right now."
	^ IceConflictingOperation left: anIceRemoval right: self
]

{ #category : #merging }
IceNoModification >> mergeWithOperation: anOperation [
	
	^ anOperation mergeWithLeftNoOperation: self
]
