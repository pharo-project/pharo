"
A FinderNode is an abstract class used by the tree of FinderUI
"
Class {
	#name : #FinderNode,
	#superclass : #MorphTreeNodeModel,
	#category : 'Tool-Finder'
}

{ #category : #'events handling' }
FinderNode >> addInspectMenuItem: menu [

	menu
		add: 'Inspect (i)'
		target: self
		selector: #inspectItem.
]

{ #category : #private }
FinderNode >> browse [

	self subclassResponsibility
]

{ #category : #private }
FinderNode >> browseClass [

	^ Smalltalk tools browser openOnClass: self item
]

{ #category : #private }
FinderNode >> displayString [
	
	^ self item asString
]

{ #category : #private }
FinderNode >> doubleClick [
	self browse
]

{ #category : #testing }
FinderNode >> hasParentNode [
	^ self parentNode isNil not
]

{ #category : #private }
FinderNode >> inspectItem [
	self item inspect
]

{ #category : #'events handling' }
FinderNode >> keyStroke: anEvent [
	(anEvent controlKeyPressed or: [ anEvent commandKeyPressed ]) ifFalse: [ ^ false ].
	anEvent keyCharacter == $b ifTrue: [ ^ self browse ].
	anEvent keyCharacter == $i ifTrue: [ ^ self inspectItem ].
]

{ #category : #'events handling' }
FinderNode >> menu: menu shifted: aBoolean [
	menu
		add: 'Browse (b)'
		target: self
		selector: #browse.
	self addInspectMenuItem: menu.
		
	menu addLine.
	menu
		add: 'Expand All'
		target: self model
		selector: #expandAll.
	menu
		add: 'Collapse All'
		target: self model
		selector: #collapseAll.
	^menu
]
