"
I am a modification in a tree. I contain the definition I'm changing (right) and the new (left) version of the definition. I know what is the icon for modification and how to calculate a conflict with other operations.
"
Class {
	#name : #IceModification,
	#superclass : #IceOperation,
	#instVars : [
		'oldNode'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #'instance creation' }
IceModification class >> left: leftDefinition right: rightDefinition [

	^ self new
		leftDefinition: leftDefinition;
		rightDefinition: rightDefinition;
		yourself
]

{ #category : #comparing }
IceModification >> = anotherOperation [

	^ super = anotherOperation
		and: [ self rightDefinition = anotherOperation rightDefinition ]
]

{ #category : #visiting }
IceModification >> accept: aVisitor [
	
	^ aVisitor visitModification: self
]

{ #category : #accessing }
IceModification >> description [
	
	^ definition name
]

{ #category : #operations }
IceModification >> inverse [
	
	^ IceModification left: self rightDefinition right: self leftDefinition
]

{ #category : #testing }
IceModification >> isModification [
	
	^ true
]

{ #category : #accessing }
IceModification >> key [
	
	^ definition key
]

{ #category : #accessing }
IceModification >> leftContents [
	
	^ definition contents
]

{ #category : #merging }
IceModification >> mergeWithLeftAddition: anIceAddition [ 
	
	"This should not happen. If I represent a modification of a node, that means that the original definition existed on the base version.
	Thus, the right version can never ADD an already existing version. It can remove it or modify it."
	self shouldNotImplement
]

{ #category : #merging }
IceModification >> mergeWithLeftModification: anIceModification [ 
	
	"If the modification is the same, I can just keep any of them."
	anIceModification leftDefinition = self leftDefinition 
		ifTrue: [ ^ IceNonConflictingOperation operation: anIceModification ].
		
	^ IceConflictingOperation left: anIceModification right: self
]

{ #category : #merging }
IceModification >> mergeWithLeftNoOperation: anIceNoModification [ 
	
	^ IceNonConflictingOperation operation: self
]

{ #category : #merging }
IceModification >> mergeWithLeftRemoval: anIceRemoval [ 
	
	^ IceConflictingOperation left: anIceRemoval right: self
]

{ #category : #merging }
IceModification >> mergeWithOperation: anOperation [
	
	^ anOperation mergeWithLeftModification: self
]

{ #category : #accessing }
IceModification >> oldDefinition [
	
	^ oldNode
]

{ #category : #accessing }
IceModification >> oldNode [
	
	^ oldNode
]

{ #category : #accessing }
IceModification >> oldNode: aNode [
	
	oldNode := aNode
]

{ #category : #accessing }
IceModification >> rightContents [
	
	^ oldNode contents
]

{ #category : #accessing }
IceModification >> rightDefinition [
	
	^ oldNode
]

{ #category : #accessing }
IceModification >> rightDefinition: aDefinition [
	
	oldNode := aDefinition
]
