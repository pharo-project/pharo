"
I am the baseclass for a variable-name refactoring change. I know about the
class(name) for this variable renaming, whether it is on the instance or class side and the old and
new variable name. See my subclasses for the concrete implementation of renaming a class or an instance variable.

Executing this refactory change will change the variable name of the (global) class.
"
Class {
	#name : 'RBRenameVariableChange',
	#superclass : 'RBCompositeRefactoryChange',
	#instVars : [
		'className',
		'isMeta'
	],
	#category : 'Refactoring-Changes-Changes-Variables',
	#package : 'Refactoring-Changes',
	#tag : 'Changes-Variables'
}

{ #category : 'instance creation' }
RBRenameVariableChange class >> rename: oldName to: newName in: aClass [
	^ self new
		oldName: oldName;
		newName: newName;
		changeClass: aClass;
		yourself
]

{ #category : 'comparing' }
RBRenameVariableChange >> = aRenameVariableChange [

	self class = aRenameVariableChange class
		ifFalse: [ ^ false ].
	^ className = aRenameVariableChange changeClassName
		and: [ isMeta = aRenameVariableChange isMeta
		and: [ oldName = aRenameVariableChange oldName
		and: [ newName = aRenameVariableChange newName ] ] ]
]

{ #category : 'private' }
RBRenameVariableChange >> addNewVariable [

	self subclassResponsibility
]

{ #category : 'accessing' }
RBRenameVariableChange >> changeClass [

	| class |
	class := onSystemEnvironment classNamed: self changeClassName.
	^ isMeta
		  ifTrue: [ class class ]
		  ifFalse: [ class ]
]

{ #category : 'accessing' }
RBRenameVariableChange >> changeClass: aBehavior [

	isMeta := aBehavior isMeta.
	className := isMeta
		             ifTrue: [ aBehavior soleInstance name ]
		             ifFalse: [ aBehavior name ]
]

{ #category : 'accessing' }
RBRenameVariableChange >> changeClassName [

	^ className
]

{ #category : 'accessing' }
RBRenameVariableChange >> changeClassName: aSymbol [

	className := aSymbol.
	isMeta ifNil: [ isMeta := false ]
]

{ #category : 'printing' }
RBRenameVariableChange >> changeString [

	^ 'Rename ' , oldName , ' to ' , newName
]

{ #category : 'private' }
RBRenameVariableChange >> copyOldValuesToNewVariable [

	self subclassResponsibility
]

{ #category : 'printing' }
RBRenameVariableChange >> displayClassName [

	^ isMeta
		ifTrue: [ self changeClassName , ' class' ]
		ifFalse: [ self changeClassName asString ]
]

{ #category : 'private' }
RBRenameVariableChange >> executeNotifying: aBlock [

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
RBRenameVariableChange >> hash [

	^ (self class hash bitXor: self oldName hash) bitXor: self newName hash
]

{ #category : 'private' }
RBRenameVariableChange >> isMeta [

	^ isMeta
]

{ #category : 'accessing' }
RBRenameVariableChange >> nameToDisplay [

	^ 'Rename ', self oldName, ' into ', self newName
]

{ #category : 'accessing' }
RBRenameVariableChange >> newName [

	^ newName
]

{ #category : 'private' }
RBRenameVariableChange >> newName: aString [

	newName := aString
]

{ #category : 'accessing' }
RBRenameVariableChange >> oldName [

	^ oldName
]

{ #category : 'private' }
RBRenameVariableChange >> oldName: aString [

	oldName := aString
]

{ #category : 'printing' }
RBRenameVariableChange >> printOn: aStream [

	aStream nextPutAll: self displayString
]

{ #category : 'accessing' }
RBRenameVariableChange >> removeOldVariable [

	self subclassResponsibility
]

{ #category : 'accessing' }
RBRenameVariableChange >> textToDisplay [

	^ self printString
]

{ #category : 'accessing' }
RBRenameVariableChange >> whatToDisplayIn: aChangeBrowser [

	| result |
	result := OrderedCollection with: self.
	^ result, (self changes flatCollect: [:e | e changes ])
]
