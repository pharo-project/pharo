"
Example for a class list node
"
Class {
	#name : #ClassListNodeExample,
	#superclass : #MorphTreeNodeModel,
	#category : #'Morphic-Examples-MorphTreeWidget'
}

{ #category : #accessing }
ClassListNodeExample >> browseItem [
	Smalltalk tools browser openOnClass: self item
]

{ #category : #menu }
ClassListNodeExample >> classButton [
	^ ( self theme 
				newButtonIn: self currentWorld
				for: self
				getState: nil
				action: #browseItem
				arguments: {}
				getEnabled: #enabled
				getLabel: nil
				help: 'Open a browser on ' translated , self item name)
				label: (self theme windowLabelForText: (self item name) , '...');
				yourself
]

{ #category : #menu }
ClassListNodeExample >> commentText [
	^ ( self theme  newTextIn: self currentWorld text: self item comment) unlock; wrapFlag: true; yourself
]

{ #category : #accessing }
ClassListNodeExample >> exploreItem [
	self inspect
]

{ #category : #menu }
ClassListNodeExample >> icon [
	^ self iconNamed: #smallExpertIcon
]

{ #category : #accessing }
ClassListNodeExample >> inspectItem [
	self inspect
]

{ #category : #accessing }
ClassListNodeExample >> keyStroke: anEvent from: aTreeMorph [
	| c |
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
ClassListNodeExample >> menu: menu shifted: b [ 
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
