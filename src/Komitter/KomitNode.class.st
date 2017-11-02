"
I am an abstract class representing a node in the tree
"
Class {
	#name : #KomitNode,
	#superclass : #TreeNodePresenter,
	#instVars : [
		'silent'
	],
	#category : 'Komitter-UI'
}

{ #category : #comparing }
KomitNode >> <= aKomitNode [ 

	^ self content <= aKomitNode content
]

{ #category : #comparing }
KomitNode >> = anotherNode [

	^ self species = anotherNode species
		and: [ self content = anotherNode content ]
]

{ #category : #private }
KomitNode >> checkChildren [
	
	self silentWhile: [
		(self children value allSatisfy: [ :e | e selected ])
			ifTrue: [ self selected: true ]
			ifFalse: [ self selected: false ] ]
]

{ #category : #comparing }
KomitNode >> hash [

	^ self content hash
]

{ #category : #api }
KomitNode >> icon [

	self subclassResponsibility
]

{ #category : #api }
KomitNode >> ifNotSilentDo: aBlock [

	silent ifFalse: aBlock
]

{ #category : #initialization }
KomitNode >> initialize [
	silent := false.
	super initialize.
	self hasContentToShow: true.
	self whenSelectedChanged: [ :aBoolean | self content selected: aBoolean ].
	self whenIsExpandedChanged: [ :aBoolean | self content expanded: aBoolean ].
	self
		whenContentChanged:
			[ :c | 
			self silentWhile: [ self selected: c selected ].
			self isExpanded: c expanded ].
	self hasChildren: [ self children value isEmptyOrNil not ]
]

{ #category : #api }
KomitNode >> label [

	self subclassResponsibility
]

{ #category : #api }
KomitNode >> silentWhile: aBlock [
	| oldSilent |
	
	oldSilent := silent.
	silent := true.
	aBlock value.
	silent := oldSilent
]
