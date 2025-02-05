Class {
	#name : 'KMCategoryAll',
	#superclass : 'Object',
	#instVars : [
		'categoryName',
		'shortcuts'
	],
	#category : 'Keymapping-Tools-Spec',
	#package : 'Keymapping-Tools-Spec'
}

{ #category : 'copying' }
KMCategoryAll >> , aKMCategory [

	self shortcuts addAll: aKMCategory allEntries keymaps
]

{ #category : 'comparing' }
KMCategoryAll >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ categoryName = anObject categoryName and: [
		  shortcuts = anObject shortcuts ]
]

{ #category : 'adding' }
KMCategoryAll >> addAll: aCollection [ 
	"Add all shortcuts in aCollection to the receiver"
	
	self shortcuts addAll: aCollection
]

{ #category : 'accessing' }
KMCategoryAll >> categoryName [

	^ categoryName
]

{ #category : 'accessing' }
KMCategoryAll >> categoryName: anObject [

	categoryName := anObject
]

{ #category : 'comparing' }
KMCategoryAll >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ categoryName hash bitXor: shortcuts hash
]

{ #category : 'accessing' }
KMCategoryAll >> icon [

	^ self iconNamed: self systemIconName
]

{ #category : 'accessing' }
KMCategoryAll >> model [
	"Required by <SpDropListPresenter> ?"

	^ self
]

{ #category : 'accessing' }
KMCategoryAll >> name [

	^  self categoryName
]

{ #category : 'printing' }
KMCategoryAll >> printOn: aStream [
	"Generate a string representation of the receiver based on its instance variables."

	super printOn: aStream.
	aStream
		nextPutAll: ' [';
		print: categoryName;
		nextPutAll: ']'
]

{ #category : 'accessing' }
KMCategoryAll >> shortcuts [

	^ shortcuts
		ifNil: [ shortcuts := OrderedCollection new ]
]

{ #category : 'accessing' }
KMCategoryAll >> shortcuts: anObject [

	shortcuts := anObject
]
