"
I am a refactory change for a class-rename refactoring. I hold the old and new name.
Executing this change will rename the (global) defined ""oldName"" class to ""newName"".
"
Class {
	#name : 'RBRenameClassChange',
	#superclass : 'RBCompositeRefactoryChange',
	#category : 'Refactoring-Changes-Changes-Classes',
	#package : 'Refactoring-Changes',
	#tag : 'Changes-Classes'
}

{ #category : 'instance creation' }
RBRenameClassChange class >> rename: oldString to: newString [

	^ self new
		  rename: oldString to: newString;
		  yourself
]

{ #category : 'comparing' }
RBRenameClassChange >> = aRenameClassChange [

	super class = aRenameClassChange class
		ifFalse: [ ^ false ].
	^oldName = aRenameClassChange oldName
		and: [ newName = aRenameClassChange newName ]
]

{ #category : 'accessing' }
RBRenameClassChange >> changeClass [

	^ (onSystemEnvironment classNamed: oldName) ifNil: [
		  onSystemEnvironment classNamed: newName ]
]

{ #category : 'private' }
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

{ #category : 'comparing' }
RBRenameClassChange >> hash [

	^ (self class hash bitXor: self oldName hash) bitXor:
		  self newName hash
]

{ #category : 'accessing' }
RBRenameClassChange >> nameToDisplay [

	^ self printString
]

{ #category : 'accessing' }
RBRenameClassChange >> newName [

	^ newName
]

{ #category : 'accessing' }
RBRenameClassChange >> oldName [

	^ oldName
]

{ #category : 'accessing' }
RBRenameClassChange >> oldVersionTextToDisplay [

	^ String streamContents: [ :s |
			self changes do: [ :c | 	s << c oldVersionTextToDisplay; cr ]]
]

{ #category : 'printing' }
RBRenameClassChange >> printOn: aStream [

	aStream
		nextPutAll: self oldName;
		nextPutAll: ' rename: ';
		print: self newName
]

{ #category : 'initialization' }
RBRenameClassChange >> rename: oldString to: newString [

	oldName := oldString.
	newName := newString
]

{ #category : 'accessing' }
RBRenameClassChange >> renameChangesForClass: oldClassName to: newClassName [

	| change |
	change := super renameChangesForClass: oldClassName to: newClassName.
	oldName asSymbol = oldClassName ifTrue: [
		change rename: newClassName to: newName ].
	^ change
]

{ #category : 'accessing' }
RBRenameClassChange >> textToDisplay [

	^ String streamContents: [ :s |
         s << self printString ; cr.
         self changes do: [ :c | 	s << c textToDisplay; cr ]]
]

{ #category : 'accessing' }
RBRenameClassChange >> whatToDisplayIn: aChangeBrowser [

	^ { self }
]
