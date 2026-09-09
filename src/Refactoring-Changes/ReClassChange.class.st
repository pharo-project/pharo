"
I am the abstract baseclass for a ""class change"" refactoring change. I know about the
class(name) whether it is a change for for the meta or the instance side.

I keep track of the undo and redo operation, but the concrete class change operation is implement by my subclasses.
"
Class {
	#name : 'ReClassChange',
	#superclass : 'ReChange',
	#instVars : [
		'className',
		'isMeta'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'testing' }
ReClassChange class >> isAbstract [

	^ self name = #ReClassChange
]

{ #category : 'comparing' }
ReClassChange >> = aRefactoryClassChange [

	self class = aRefactoryClassChange class ifFalse: [ ^ false ].
	^ className = aRefactoryClassChange changeClassName and: [
		  isMeta = aRefactoryClassChange isMeta ]
]

{ #category : 'converting' }
ReClassChange >> asUndoOperation [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ReClassChange >> changeClass [

	| class |
	class := onSystemEnvironment
		         at: self changeClassName
		         ifAbsent: [ ^ nil ].
	^ isMeta
		  ifTrue: [ class classSide ]
		  ifFalse: [ class ]
]

{ #category : 'accessing' }
ReClassChange >> changeClass: aBehavior [

	isMeta := aBehavior isMeta.
	className := aBehavior instanceSide name
]

{ #category : 'accessing' }
ReClassChange >> changeClassName [

	^ className
]

{ #category : 'accessing' }
ReClassChange >> changeClassName: aSymbol [

	className := aSymbol.
	isMeta ifNil: [ isMeta := false ]
]

{ #category : 'printing' }
ReClassChange >> changeString [

	^ self displayClassName
]

{ #category : 'printing' }
ReClassChange >> displayClassName [

	^ isMeta
		  ifTrue: [ self changeClassName , ' class' ]
		  ifFalse: [ self changeClassName asString ]
]

{ #category : 'private' }
ReClassChange >> executeNotifying: aBlock [

	| undo |
	undo := self asUndoOperation.
	undo name: self name.
	self generateChanges.
	aBlock value.
	^ undo
]

{ #category : 'private' }
ReClassChange >> generateChanges [

	^ self subclassResponsibility
]

{ #category : 'comparing' }
ReClassChange >> hash [

	^ self changeClassName hash
]

{ #category : 'testing' }
ReClassChange >> isInScope: aRBBrowserEnvironment [ 
	^ aRBBrowserEnvironment includesClass: self changeClass.
]

{ #category : 'private' }
ReClassChange >> isMeta [

	^ isMeta
]

{ #category : 'accessing' }
ReClassChange >> methodSourceFor: aSymbol [

	^ (self changeClass includesSelector: aSymbol) ifTrue: [
		  (self changeClass >> aSymbol) sourceCode ]
]

{ #category : 'accessing' }
ReClassChange >> parserClass [

	^ OCParser
]

{ #category : 'printing' }
ReClassChange >> printOn: aStream [

	aStream nextPutAll: self displayString
]

{ #category : 'accessing' }
ReClassChange >> renameChangesForClass: oldClassName to: newClassName [

	^ self changeClassName = oldClassName
		  ifFalse: [ self ]
		  ifTrue: [
			  self copy
				  changeClassName: newClassName;
				  yourself ]
]
