"
Example for abstract package nodes
"
Class {
	#name : #PackageAbstractNodeExample,
	#superclass : #MorphTreeNodeModel,
	#instVars : [
		'contents'
	],
	#category : #'Morphic-Examples-MorphTreeWidget'
}

{ #category : #accessing }
PackageAbstractNodeExample >> browseItem [
	
]

{ #category : #menu }
PackageAbstractNodeExample >> contents [
	^ contents ifNil: [contents := super contents]
]

{ #category : #accessing }
PackageAbstractNodeExample >> doubleClick [
	self browseItem
]

{ #category : #accessing }
PackageAbstractNodeExample >> exploreItem [
	self inspect
]

{ #category : #accessing }
PackageAbstractNodeExample >> inspectItem [
	self inspect
]

{ #category : #accessing }
PackageAbstractNodeExample >> keyStroke: anEvent from: aTreeMorph [
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
PackageAbstractNodeExample >> menu: menu shifted: b [ 
	menu
		add: 'Expand all' 
		target: self
		selector: #expandAll.
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
