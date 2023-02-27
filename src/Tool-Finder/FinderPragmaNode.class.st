"
A FinderPragmaNode is a Node used to render a pragma in the Finder Tree
"
Class {
	#name : #FinderPragmaNode,
	#superclass : #FinderNode,
	#category : #'Tool-Finder-Tree'
}

{ #category : #private }
FinderPragmaNode >> browse [
	Smalltalk tools messageList browseSendersOf: self item
]

{ #category : #private }
FinderPragmaNode >> browseClass [
]

{ #category : #private }
FinderPragmaNode >> browseHierarchy [
]

{ #category : #private }
FinderPragmaNode >> browseReferences [

	self systemNavigation browseAllReferencesTo: self item
]

{ #category : #accessing }
FinderPragmaNode >> childNodeClassFromItem: anItem [

	^ FinderPragmaMethodNode
]

{ #category : #accessing }
FinderPragmaNode >> childrenItems [
	"I search the children, if I have not got any, I call my super method"

	^ (self model resultDictionary at: self item ifAbsent:[^super childrenItems]) keys
]

{ #category : #private }
FinderPragmaNode >> displayString [
	^ '<' , self item  , '>'
]

{ #category : #private }
FinderPragmaNode >> inspectItem [
	(RBBrowserEnvironment default forPragmas: {self item}) methods inspect
]

{ #category : #'event handling' }
FinderPragmaNode >> keyDown: anEvent [
	(anEvent controlKeyPressed or: [ anEvent commandKeyPressed ]) ifFalse: [ ^ false ].
	anEvent keyCharacter == $h ifTrue: [ ^ self browseHierarchy ].
	anEvent keyCharacter == $N ifTrue: [ ^ self browseReferences ].
	^ super keyDown: anEvent
]

{ #category : #'event handling' }
FinderPragmaNode >> menu: menu shifted: aBoolean [
	menu
		add: 'References (N)'
		target: self
		selector: #browseReferences.
	^ super menu: menu shifted: aBoolean
]
