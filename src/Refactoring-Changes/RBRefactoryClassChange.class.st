"
I am the abstract baseclass for a ""class change"" refactoring change. I know about the
class(name) whether it is a change for for the meta or the instance side.

I keep track of the undo and redo operation, but the concrete class change operation is implement by my subclasses.
"
Class {
	#name : 'RBRefactoryClassChange',
	#superclass : 'RBRefactoryChange',
	#instVars : [
		'className',
		'isMeta'
	],
	#category : 'Refactoring-Changes-Changes-Classes',
	#package : 'Refactoring-Changes',
	#tag : 'Changes-Classes'
}

{ #category : 'testing' }
RBRefactoryClassChange class >> isAbstract [

	^ self name = #RBRefactoryClassChange
]

{ #category : 'comparing' }
RBRefactoryClassChange >> = aRefactoryClassChange [

	self class = aRefactoryClassChange class ifFalse: [ ^ false ].
	^ className = aRefactoryClassChange changeClassName and: [
		  isMeta = aRefactoryClassChange isMeta ]
]

{ #category : 'converting' }
RBRefactoryClassChange >> asUndoOperation [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
RBRefactoryClassChange >> changeClass [

	| class |
	class := onSystemEnvironment
		         at: self changeClassName
		         ifAbsent: [ ^ nil ].
	^ isMeta
		  ifTrue: [ class classSide ]
		  ifFalse: [ class ]
]

{ #category : 'accessing' }
RBRefactoryClassChange >> changeClass: aBehavior [

	isMeta := aBehavior isMeta.
	className := aBehavior instanceSide name
]

{ #category : 'accessing' }
RBRefactoryClassChange >> changeClassName [

	^ className
]

{ #category : 'accessing' }
RBRefactoryClassChange >> changeClassName: aSymbol [

	className := aSymbol.
	isMeta ifNil: [ isMeta := false ]
]

{ #category : 'printing' }
RBRefactoryClassChange >> changeString [

	^ self displayClassName
]

{ #category : 'printing' }
RBRefactoryClassChange >> displayClassName [

	^ isMeta
		  ifTrue: [ self changeClassName , ' class' ]
		  ifFalse: [ self changeClassName asString ]
]

{ #category : 'private' }
RBRefactoryClassChange >> executeNotifying: aBlock [

	| undo |
	undo := self asUndoOperation.
	undo name: self name.
	self generateChanges.
	aBlock value.
	^ undo
]

{ #category : 'private' }
RBRefactoryClassChange >> generateChanges [

	^ self subclassResponsibility
]

{ #category : 'comparing' }
RBRefactoryClassChange >> hash [

	^ self changeClassName hash
]

{ #category : 'private' }
RBRefactoryClassChange >> isMeta [

	^ isMeta
]

{ #category : 'accessing' }
RBRefactoryClassChange >> methodSourceFor: aSymbol [

	^ (self changeClass includesSelector: aSymbol) ifTrue: [
		  self changeClass sourceCodeAt: aSymbol ]
]

{ #category : 'accessing' }
RBRefactoryClassChange >> parserClass [

	^ RBParser
]

{ #category : 'printing' }
RBRefactoryClassChange >> printOn: aStream [

	aStream nextPutAll: self displayString
]

{ #category : 'accessing' }
RBRefactoryClassChange >> renameChangesForClass: oldClassName to: newClassName [

	^ self changeClassName = oldClassName
		  ifFalse: [ self ]
		  ifTrue: [
			  self copy
				  changeClassName: newClassName;
				  yourself ]
]
