Class {
	#name : 'KMCategoryItemPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'categoryName',
		'shortcuts'
	],
	#category : 'Keymapping-Tools-Spec',
	#package : 'Keymapping-Tools-Spec'
}

{ #category : 'copying' }
KMCategoryItemPresenter >> , aKMCategory [

	self shortcuts addAll: aKMCategory allEntries keymaps asOrderedCollection

]

{ #category : 'comparing' }
KMCategoryItemPresenter >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ categoryName = anObject categoryName and: [
		  shortcuts = anObject shortcuts ]
]

{ #category : 'adding' }
KMCategoryItemPresenter >> addAll: aCollection [ 
	"Add all shortcuts in aCollection to the receiver"
	
	self shortcuts addAll: aCollection
]

{ #category : 'accessing' }
KMCategoryItemPresenter >> categoryName [

	^ categoryName
]

{ #category : 'accessing' }
KMCategoryItemPresenter >> categoryName: anObject [

	categoryName := anObject
]

{ #category : 'comparing' }
KMCategoryItemPresenter >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ categoryName hash bitXor: shortcuts hash
]

{ #category : 'accessing' }
KMCategoryItemPresenter >> icon [

	^ self iconNamed: self systemIconName
]

{ #category : 'accessing' }
KMCategoryItemPresenter >> model [
	"Required by <SpDropListPresenter> ?"

	^ self
]

{ #category : 'accessing' }
KMCategoryItemPresenter >> name [

	^  self categoryName
]

{ #category : 'printing' }
KMCategoryItemPresenter >> printOn: aStream [
	"Generate a string representation of the receiver based on its instance variables."

	super printOn: aStream.
	aStream
		nextPutAll: ' [';
		print: categoryName;
		nextPutAll: ']'
]

{ #category : 'accessing' }
KMCategoryItemPresenter >> shortcuts [

	^ shortcuts
		ifNil: [ shortcuts := OrderedCollection new ]
]

{ #category : 'accessing' }
KMCategoryItemPresenter >> shortcuts: anObject [

	shortcuts := anObject
]
