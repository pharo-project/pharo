"
I represent a merge between two operations. I know if I'm a conflict or not, if I have changes, and the user can use me to resolve a conflict.
"
Class {
	#name : #IceOperationMerge,
	#superclass : #Object,
	#instVars : [
		'chosen'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #visiting }
IceOperationMerge >> accept: aVisitor [
	
	^ self subclassResponsibility
]

{ #category : #'morphic-compatibility' }
IceOperationMerge >> actualClass [
	self subclassResponsibility
]

{ #category : #accessing }
IceOperationMerge >> chosenOperation [
	
	self subclassResponsibility
]

{ #category : #resolution }
IceOperationMerge >> clearSelection [
	
	chosen := nil
]

{ #category : #'morphic-compatibility' }
IceOperationMerge >> contextClass [
	self subclassResponsibility
]

{ #category : #accessing }
IceOperationMerge >> definition [
	self subclassResponsibility
]

{ #category : #accessing }
IceOperationMerge >> description [
	self subclassResponsibility
]

{ #category : #testing }
IceOperationMerge >> hasChanges [

	^ self chosenOperation hasChanges
]

{ #category : #testing }
IceOperationMerge >> isConflict [
	self subclassResponsibility
]

{ #category : #resolution }
IceOperationMerge >> isLeftChosen [
	self subclassResponsibility
]

{ #category : #'morphic-compatibility' }
IceOperationMerge >> isLoadable [
	^ false
]

{ #category : #testing }
IceOperationMerge >> isResolved [
	
	^ chosen notNil
]

{ #category : #resolution }
IceOperationMerge >> isRightChosen [
	self subclassResponsibility
]

{ #category : #accessing }
IceOperationMerge >> key [
	self subclassResponsibility
]

{ #category : #accessing }
IceOperationMerge >> leftContents [
	self subclassResponsibility
]

{ #category : #accessing }
IceOperationMerge >> rightContents [
	self subclassResponsibility
]

{ #category : #resolution }
IceOperationMerge >> selectLeft [
	self subclassResponsibility
]

{ #category : #resolution }
IceOperationMerge >> selectRight [
	self subclassResponsibility
]
