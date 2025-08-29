"
I am the baseclass for a variable-name refactoring change. I know about the
class(name) for this variable renaming, whether it is on the instance or class side and the old and
new variable name. See my subclasses for the concrete implementation of renaming a class or an instance variable.

Executing this refactory change will change the variable name of the (global) class.
"
Class {
	#name : 'ReRenameVariableChange',
	#superclass : 'ReCompositeChange',
	#instVars : [
		'className',
		'isMeta'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
ReRenameVariableChange class >> rename: oldName to: newName in: aClass [
	^ self new
		oldName: oldName;
		newName: newName;
		changeClass: aClass;
		yourself
]

{ #category : 'comparing' }
ReRenameVariableChange >> = aRenameVariableChange [

	self class = aRenameVariableChange class
		ifFalse: [ ^ false ].
	^ className = aRenameVariableChange changeClassName
		and: [ isMeta = aRenameVariableChange isMeta
		and: [ oldName = aRenameVariableChange oldName
		and: [ newName = aRenameVariableChange newName ] ] ]
]

{ #category : 'private' }
ReRenameVariableChange >> addNewVariable [

	self subclassResponsibility
]

{ #category : 'accessing' }
ReRenameVariableChange >> changeClass [

	| class |
	class := onSystemEnvironment classNamed: self changeClassName.
	^ isMeta
		  ifTrue: [ class class ]
		  ifFalse: [ class ]
]

{ #category : 'accessing' }
ReRenameVariableChange >> changeClass: aBehavior [

	isMeta := aBehavior isMeta.
	className := isMeta
		             ifTrue: [ aBehavior soleInstance name ]
		             ifFalse: [ aBehavior name ]
]

{ #category : 'accessing' }
ReRenameVariableChange >> changeClassName [

	^ className
]

{ #category : 'accessing' }
ReRenameVariableChange >> changeClassName: aSymbol [

	className := aSymbol.
	isMeta ifNil: [ isMeta := false ]
]

{ #category : 'printing' }
ReRenameVariableChange >> changeString [

	^ 'Rename ' , oldName , ' to ' , newName
]

{ #category : 'accessing' }
ReRenameVariableChange >> changesToDisplayIn: aChangeBrowser [

	
	^ { self } , (self changes flatCollect: [:e | e changes ])
]

{ #category : 'private' }
ReRenameVariableChange >> copyOldValuesToNewVariable [

	self subclassResponsibility
]

{ #category : 'printing' }
ReRenameVariableChange >> displayClassName [

	^ isMeta
		ifTrue: [ self changeClassName , ' class' ]
		ifFalse: [ self changeClassName asString ]
]

{ #category : 'private' }
ReRenameVariableChange >> executeNotifying: aBlock [

	| undo |
	self addNewVariable.
	self copyOldValuesToNewVariable.
	undo := super executeNotifying: aBlock.
	undo
		oldName: newName;
		newName: oldName.
	self removeOldVariable.
	^ undo
]

{ #category : 'comparing' }
ReRenameVariableChange >> hash [

	^ (self class hash bitXor: self oldName hash) bitXor: self newName hash
]

{ #category : 'private' }
ReRenameVariableChange >> isMeta [

	^ isMeta
]

{ #category : 'accessing' }
ReRenameVariableChange >> nameToDisplay [

	^ 'Rename ', self oldName, ' into ', self newName
]

{ #category : 'accessing' }
ReRenameVariableChange >> newName [

	^ newName
]

{ #category : 'private' }
ReRenameVariableChange >> newName: aString [

	newName := aString
]

{ #category : 'accessing' }
ReRenameVariableChange >> oldName [

	^ oldName
]

{ #category : 'private' }
ReRenameVariableChange >> oldName: aString [

	oldName := aString
]

{ #category : 'printing' }
ReRenameVariableChange >> printOn: aStream [

	aStream nextPutAll: self displayString
]

{ #category : 'accessing' }
ReRenameVariableChange >> removeOldVariable [

	self subclassResponsibility
]

{ #category : 'accessing' }
ReRenameVariableChange >> textToDisplay [

	^ self printString
]
