"
I am a refactory change for a class-rename refactoring. I hold the old and new name.
Executing this change will rename the (global) defined ""oldName"" class to ""newName"".

I also hold the changes that update the references to the class. The model records them with the old class name, so they must stay inside me:
- when executing, I rename the class first, then move each change to the new name (`renameChangesForClass:to:`) and execute it;
- my undo renames the class back first, then restores the old sources.

If these changes are executed on their own, next to me, they point to a class name that no longer exists. Renaming then fails (see issue #20238), and undo fails to compile the old sources.
"
Class {
	#name : 'ReRenameClassChange',
	#superclass : 'ReCompositeChange',
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
ReRenameClassChange class >> rename: oldString to: newString [

	^ self new
		  rename: oldString to: newString;
		  yourself
]

{ #category : 'comparing' }
ReRenameClassChange >> = aRenameClassChange [

	super class = aRenameClassChange class
		ifFalse: [ ^ false ].
	^oldName = aRenameClassChange oldName
		and: [ newName = aRenameClassChange newName ]
]

{ #category : 'accessing' }
ReRenameClassChange >> changeClass [

	^ (onSystemEnvironment classNamed: oldName) ifNil: [
		  onSystemEnvironment classNamed: newName ]
]

{ #category : 'private' }
ReRenameClassChange >> changeString [
	
	^ self textToDisplay
]

{ #category : 'accessing' }
ReRenameClassChange >> changesToDisplayIn: aChangeBrowser [

	^ { self }
]

{ #category : 'private' }
ReRenameClassChange >> executeNotifying: aBlock [

	| undos |
	self changeClass rename: newName.
	undos := changes collect: [ :each |
		(each renameChangesForClass: oldName asSymbol to: newName asSymbol)
			executeNotifying: aBlock ].
	"Cleanup of undeclared registry because during the renaming process the old class was registered as 
	undeclared, now that we updated all references we can safely remove undeclared entry."
	self changeClass undeclaredRegistry removeKey: oldName asSymbol ifAbsent: [ ].
	^ self copy
		changes: undos reverse;
		rename: newName to: oldName;
		yourself
]

{ #category : 'comparing' }
ReRenameClassChange >> hash [

	^ (self class hash bitXor: self oldName hash) bitXor:
		  self newName hash
]

{ #category : 'accessing' }
ReRenameClassChange >> nameToDisplay [

	^ self printString
]

{ #category : 'accessing' }
ReRenameClassChange >> newName [

	^ newName
]

{ #category : 'accessing' }
ReRenameClassChange >> oldName [

	^ oldName
]

{ #category : 'accessing' }
ReRenameClassChange >> oldVersionTextToDisplay [

	^ String streamContents: [ :s |
			self changes do: [ :c | 	s << c oldVersionTextToDisplay; cr ]]
]

{ #category : 'printing' }
ReRenameClassChange >> printOn: aStream [

	aStream
		nextPutAll: self oldName;
		nextPutAll: ' rename: ';
		print: self newName
]

{ #category : 'initialization' }
ReRenameClassChange >> rename: oldString to: newString [

	oldName := oldString.
	newName := newString
]

{ #category : 'accessing' }
ReRenameClassChange >> renameChangesForClass: oldClassName to: newClassName [

	| change |
	change := super renameChangesForClass: oldClassName to: newClassName.
	oldName asSymbol = oldClassName ifTrue: [
		change rename: newClassName to: newName ].
	^ change
]

{ #category : 'accessing' }
ReRenameClassChange >> textToDisplay [

	^ String streamContents: [ :s |
         s << self printString ; cr.
         self changes do: [ :c | 	s << c textToDisplay; cr ]]
]
