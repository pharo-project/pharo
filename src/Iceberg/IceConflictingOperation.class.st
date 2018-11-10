"
I reprensent a conflict between two operations that can be solved by using #selectLeft and #selectRight.
"
Class {
	#name : #IceConflictingOperation,
	#superclass : #IceOperationMerge,
	#instVars : [
		'leftOperation',
		'rightOperation'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #'instance creation' }
IceConflictingOperation class >> left: leftOperation right: rightOperation [
	
	^ self new
		leftOperation: leftOperation;
		rightOperation: rightOperation;
		yourself
]

{ #category : #visiting }
IceConflictingOperation >> accept: aVisitor [
	
	^ aVisitor visitConflictingOperation: self
]

{ #category : #'morphic-compatibility' }
IceConflictingOperation >> actualClass [
	
	^ leftOperation definition actualClass
]

{ #category : #resolution }
IceConflictingOperation >> chosenOperation [

	^ chosen
]

{ #category : #'morphic-compatibility' }
IceConflictingOperation >> contextClass [
	
	^ leftOperation contextClass
]

{ #category : #accessing }
IceConflictingOperation >> definition [
	
	"This is just for compatibility"
	^ leftOperation definition
]

{ #category : #accessing }
IceConflictingOperation >> description [
	
	"Any of my instances should have the same description"
	^ leftOperation description
]

{ #category : #testing }
IceConflictingOperation >> isConflict [

	^ true
]

{ #category : #resolution }
IceConflictingOperation >> isLeftChosen [
	
	^ chosen = leftOperation
]

{ #category : #resolution }
IceConflictingOperation >> isRightChosen [
	
	^ chosen = rightOperation
]

{ #category : #accessing }
IceConflictingOperation >> key [
	
	"Any of my operations should have the same key"
	^ leftOperation key
]

{ #category : #accessing }
IceConflictingOperation >> leftContents [
	
	^ leftOperation leftContents
]

{ #category : #accessing }
IceConflictingOperation >> leftOperation: anOperation [
	
	leftOperation := anOperation
]

{ #category : #accessing }
IceConflictingOperation >> rightContents [
	
	^ rightOperation leftContents
]

{ #category : #accessing }
IceConflictingOperation >> rightOperation: anOperation [
	
	rightOperation := anOperation
]

{ #category : #resolution }
IceConflictingOperation >> selectLeft [
	
	chosen := leftOperation
]

{ #category : #resolution }
IceConflictingOperation >> selectRight [
	
	chosen := rightOperation
]
