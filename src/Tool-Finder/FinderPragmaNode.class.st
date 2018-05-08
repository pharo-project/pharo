"
A FinderPragmaNode is a Node used to render a pragma in the Finder Tree
"
Class {
	#name : #FinderPragmaNode,
	#superclass : #FinderNode,
	#category : #'Tool-Finder'
}

{ #category : #private }
FinderPragmaNode >> browse [
	Smalltalk tools browser openInEnvironment: (RBBrowserEnvironment default forPragmas:{ self item})
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
FinderPragmaNode >> foundReceiverOf: aString [
	"It's ugly, but I haven't found a method that allows me to do that easily"

	| selector index firstPart |
	selector := (self model finder findSelector: aString).
	index := selector indexOf: $:.
	firstPart := selector.
	(index = 0)
		ifFalse: [firstPart := selector copyFrom: 1 to: index].
	index :=  aString findString: firstPart.
	^ aString copyFrom: 1 to: (index-1)
]

{ #category : #private }
FinderPragmaNode >> inspectItem [
	(RBBrowserEnvironment default forPragmas: {self item}) methods inspect
]

{ #category : #'event handling' }
FinderPragmaNode >> keyStroke: anEvent [
	(anEvent controlKeyPressed or: [ anEvent commandKeyPressed ]) ifFalse: [ ^ false ].
	anEvent keyCharacter == $h ifTrue: [ ^ self browseHierarchy ].
	anEvent keyCharacter == $N ifTrue: [ ^ self browseReferences ].
	^ super keyStroke: anEvent
]

{ #category : #'event handling' }
FinderPragmaNode >> menu: menu shifted: aBoolean [
	menu
		add: 'References (N)'
		target: self
		selector: #browseReferences.
	^ super menu: menu shifted: aBoolean
]
