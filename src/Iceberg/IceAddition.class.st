"
I am an addition in a tree. I contain the definition I'm adding. I know what is the icon for addition and how to calculate a conflict with other operations.
"
Class {
	#name : #IceAddition,
	#superclass : #IceOperation,
	#category : #'Iceberg-Changes'
}

{ #category : #visiting }
IceAddition >> accept: aVisitor [
	
	^ aVisitor visitAddition: self
]

{ #category : #operations }
IceAddition >> inverse [
	
	^ IceRemoval definition: definition
]

{ #category : #testing }
IceAddition >> isAddition [
	
	^ true
]

{ #category : #contents }
IceAddition >> leftContents [
	
	^ definition contents
]

{ #category : #merging }
IceAddition >> mergeWithLeftAddition: anIceAddition [ 
	
	"If the addition is the same, I can just keep any of them."
	anIceAddition definition = self definition 
		ifTrue: [ ^ IceNonConflictingOperation operation: anIceAddition ].
		
	^ IceConflictingOperation left: anIceAddition right: self
]

{ #category : #merging }
IceAddition >> mergeWithLeftModification: anIceModification [ 
	
	"This should not happen. If I represent an addition of a node, that means that the original definition did not exist on the base version.
	Thus, the left version can never MODIFY a non existing version. It can add it too."
	self shouldNotImplement
]

{ #category : #merging }
IceAddition >> mergeWithLeftNoOperation: anIceNoModification [ 
	
	"This should not happen. If I represent an addition of a node, that means that the original definition did not exist on the base version.
	Thus, the left version can never have an operation on the same element."
	self shouldNotImplement
]

{ #category : #merging }
IceAddition >> mergeWithLeftRemoval: anIceRemoval [ 
	
	"This should not happen. If I represent an addition of a node, that means that the original definition did not exist on the base version.
	Thus, the left version can never REMOVE a non existing version. It can add it too."
	self shouldNotImplement
]

{ #category : #merging }
IceAddition >> mergeWithOperation: anOperation [
	
	^ anOperation mergeWithLeftAddition: self
]

{ #category : #accessing }
IceAddition >> rightContents [
	
	^ ''
]
