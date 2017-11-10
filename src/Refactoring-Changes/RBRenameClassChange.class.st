"
I am a refactory change for a class-rename refactoring. I hold the old and new name.
Executing this change will rename the (global) defined ""oldName"" class to ""newName"".
"
Class {
	#name : #RBRenameClassChange,
	#superclass : #RBCompositeRefactoryChange,
	#instVars : [
		'oldName',
		'newName'
	],
	#category : #Refactoring-Changes
}

{ #category : #'instance creation' }
RBRenameClassChange class >> rename: oldString to: newString [ 
	^ self new
		rename: oldString to: newString;
		yourself
]

{ #category : #comparing }
RBRenameClassChange >> = aRenameClassChange [ 
	super class = aRenameClassChange class
		ifFalse: [ ^ false ].
	^oldName = aRenameClassChange oldName 
		and: [ newName = aRenameClassChange newName ]
]

{ #category : #accessing }
RBRenameClassChange >> changeClass [
	^ Smalltalk globals at: oldName asSymbol ifAbsent: [ Smalltalk globals at: newName asSymbol ]
]

{ #category : #private }
RBRenameClassChange >> executeNotifying: aBlock [ 
	| undos |
	self changeClass rename: newName.
	undos := changes collect: [ :each | 
		(each renameChangesForClass: oldName asSymbol to: newName asSymbol) 
			executeNotifying: aBlock ].
	^ self copy
		changes: undos reverse;
		rename: newName to: oldName;
		yourself
]

{ #category : #comparing }
RBRenameClassChange >> hash [
	^ (self class hash bitXor: self oldName hash) bitXor: self newName hash
]

{ #category : #accessing }
RBRenameClassChange >> newName [
	^ newName
]

{ #category : #accessing }
RBRenameClassChange >> oldName [
	^ oldName
]

{ #category : #printing }
RBRenameClassChange >> printOn: aStream [ 
	aStream
		nextPutAll: self oldName;
		nextPutAll: ' rename: ';
		print: self newName;
		nextPut: $!
]

{ #category : #initialize-release }
RBRenameClassChange >> rename: oldString to: newString [ 
	oldName := oldString.
	newName := newString
]

{ #category : #accessing }
RBRenameClassChange >> renameChangesForClass: oldClassName to: newClassName [
	| change |
	change := super renameChangesForClass: oldClassName to: newClassName.
	oldName asSymbol = oldClassName
		ifTrue: [ change rename: newClassName to: newName ].
	^ change
]
