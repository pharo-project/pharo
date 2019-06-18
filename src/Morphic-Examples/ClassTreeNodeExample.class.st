"
Example for class tree nodes
"
Class {
	#name : #ClassTreeNodeExample,
	#superclass : #MorphTreeNodeModel,
	#instVars : [
		'contents'
	],
	#category : #'Morphic-Examples-MorphTreeWidget'
}

{ #category : #accessing }
ClassTreeNodeExample >> browseItem [
	Smalltalk tools browser openOnClass: self item
]

{ #category : #menu }
ClassTreeNodeExample >> childrenItems [
	^ (self item subclasses asArray sort: [:a :b | a name < b name]) asOrderedCollection

]

{ #category : #menu }
ClassTreeNodeExample >> classButton [
	^ ( self theme 
				newButtonIn: self currentWorld
				for: self
				getState: nil
				action: #browseItem
				arguments: {}
				getEnabled: #enabled
				getLabel: nil
				help: 'Open a browser on ' translated , self item name)
				label: (self theme  windowLabelForText: (self item name) , '...');
				yourself
]

{ #category : #menu }
ClassTreeNodeExample >> commentText [
	^ ( self theme  newTextIn: self currentWorld text: self item comment) unlock; wrapFlag: true; yourself
]

{ #category : #menu }
ClassTreeNodeExample >> contents [
	^ contents ifNil: [contents := super contents]
]

{ #category : #accessing }
ClassTreeNodeExample >> exploreItem [
	self inspect
]

{ #category : #menu }
ClassTreeNodeExample >> icon [
	^ self iconNamed: #smallExpertIcon
]

{ #category : #accessing }
ClassTreeNodeExample >> inspectItem [
	self inspect
]

{ #category : #accessing }
ClassTreeNodeExample >> keyStroke: anEvent from: aTreeMorph [ 
	| c  |
	c := anEvent keyCharacter.
	c = $b
		ifTrue: [self browseItem. ^ true].
	c = $i
		ifTrue: [self inspectItem. ^ true].
	c = $I
		ifTrue: [self exploreItem. ^ true].
	^ false

]

{ #category : #accessing }
ClassTreeNodeExample >> menu: menu shifted: b [ 
	menu
		add: 'Browse (b)'
		target: self
		selector: #browseItem.
	menu
		add: 'Inspect (i)'
		target: self
		selector: #inspectItem.
	menu
		add: 'Explore (I)'
		target: self
		selector: #exploreItem.

]
