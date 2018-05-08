"
A FinderMethodNode is a node used by the FinderUI's tree representing a Method
"
Class {
	#name : #FinderMethodNode,
	#superclass : #FinderNode,
	#category : #'Tool-Finder'
}

{ #category : #'event handling' }
FinderMethodNode >> addInspectMenuItem: menu [

	self hasParentNode ifTrue: [ ^ super addInspectMenuItem: menu ]
]

{ #category : #private }
FinderMethodNode >> browse [
	self item isSymbol	ifTrue: [ 	
		^ SystemNavigation new browseAllImplementorsOf: self item ].
	self item browse
]

{ #category : #private }
FinderMethodNode >> browseClass [

	^ Smalltalk tools browser openOnClass: self parentNode item selector: self item
]

{ #category : #private }
FinderMethodNode >> browseImplementors [
	SystemNavigation new browseAllImplementorsOf: self selector 
		
]

{ #category : #private }
FinderMethodNode >> browseSenders [

	SystemNavigation new browseSendersOf: self selector name: 'Senders of ', self selector autoSelect: self selector
]

{ #category : #accessing }
FinderMethodNode >> childNodeClassFromItem: anItem [
	"I answer the class of my children nodes (if I have some)"
	^ FinderClassNode
]

{ #category : #accessing }
FinderMethodNode >> childrenItems [
	"I search the children, if I have not got any, I call my super method"

	self model isPragmasSymbol
		ifTrue: [ ^ (self model resultDictionary at: parentNode item ifAbsent: [ ^ super childrenItems ])
				at: self item
				ifAbsent: [ ^ super childrenItems ] ].
	^ self model resultDictionary at: self item ifAbsent: [ ^ super childrenItems ]
]

{ #category : #private }
FinderMethodNode >> inspectItem [
	self hasParentNode ifTrue: [ (self parentNode item compiledMethodAt: self item) inspect ]
]

{ #category : #private }
FinderMethodNode >> isSingle [

	^ false
]

{ #category : #'event handling' }
FinderMethodNode >> keyStroke: anEvent [
	(anEvent controlKeyPressed or: [ anEvent commandKeyPressed ]) ifFalse: [ ^ false ].
	anEvent keyCharacter == $n ifTrue: [ ^ self browseSenders ].
	anEvent keyCharacter == $m ifTrue: [ ^ self browseImplementors ].
	^ super keyStroke: anEvent
]

{ #category : #'event handling' }
FinderMethodNode >> menu: menu shifted: aBoolean [
	
	menu
		add: 'Implementors (m)'
		target: self
		selector: #browseImplementors.
		
	menu
		add: 'Senders (n)'
		target: self
		selector: #browseSenders.
		
	^ super menu: menu shifted: aBoolean
]

{ #category : #private }
FinderMethodNode >> selector [
	^ self item
]
