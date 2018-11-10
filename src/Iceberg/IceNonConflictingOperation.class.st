"
I reprensent a non-conflict between two operations that can be solved automatically. The user can still override the automatic choice using #selectLeft and #selectRight.
"
Class {
	#name : #IceNonConflictingOperation,
	#superclass : #IceOperationMerge,
	#instVars : [
		'operation'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #'instance creation' }
IceNonConflictingOperation class >> operation: anOperation [
	
	^ self new
		operation: anOperation;
		yourself
]

{ #category : #visiting }
IceNonConflictingOperation >> accept: aVisitor [
	
	^ aVisitor visitNonConflictingOperation: self
]

{ #category : #'morphic-compatibility' }
IceNonConflictingOperation >> actualClass [
	
	^ operation definition actualClass
]

{ #category : #accessing }
IceNonConflictingOperation >> chosenOperation [
	
	^ chosen
		ifNil: [ operation ]
		ifNotNil: [ chosen ]
]

{ #category : #'morphic-compatibility' }
IceNonConflictingOperation >> contextClass [
	
	^ operation contextClass
]

{ #category : #accessing }
IceNonConflictingOperation >> definition [
	
	"This is just for compatibility"
	^ operation definition
]

{ #category : #accessing }
IceNonConflictingOperation >> description [
	
	"Any of my instances should have the same description"
	^ operation description
]

{ #category : #testing }
IceNonConflictingOperation >> isConflict [

	^ false
]

{ #category : #resolution }
IceNonConflictingOperation >> isLeftChosen [
	
	^ chosen = operation
]

{ #category : #resolution }
IceNonConflictingOperation >> isRightChosen [
	
	^ chosen = operation inverse
]

{ #category : #accessing }
IceNonConflictingOperation >> key [
	
	"Any of my operations should have the same key"
	^ operation key
]

{ #category : #accessing }
IceNonConflictingOperation >> leftContents [
	
	^ operation leftContents
]

{ #category : #accessing }
IceNonConflictingOperation >> operation: anIceModification [ 
	operation := anIceModification
]

{ #category : #accessing }
IceNonConflictingOperation >> rightContents [
	
	^ operation rightContents
]

{ #category : #resolution }
IceNonConflictingOperation >> selectLeft [
	
	chosen := operation
]

{ #category : #resolution }
IceNonConflictingOperation >> selectRight [
	
	chosen := operation inverse
]
