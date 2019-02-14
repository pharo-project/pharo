"
I'm a spec for a menu. The World menu pragma based registration uses it for now. I'm moved in this package so that later we can decide what to do with it. 
We could probably avoid MenuSpec and just use MenuMorph instead. 
"
Class {
	#name : #PluggableMenuSpec,
	#superclass : #Object,
	#instVars : [
		'label',
		'model',
		'items',
		'parentMenu'
	],
	#category : #'MenuRegistration-Core'
}

{ #category : #'instance creation' }
PluggableMenuSpec class >> withModel: aModel [
	^ self new model: aModel
]

{ #category : #construction }
PluggableMenuSpec >> add: aString [
	^ self add: aString action: nil
	
]

{ #category : #construction }
PluggableMenuSpec >> add: aString action: aMessageSend [
	| item |
	item := self addMenuItem.
	item label: aString translated.
	item action: aMessageSend.
	^item
]

{ #category : #construction }
PluggableMenuSpec >> add: aString selector: aMessageSend [
	| item |
	item := self addMenuItem.
	item label: aString translated.
	item action: aMessageSend.
	^item
]

{ #category : #construction }
PluggableMenuSpec >> add: aString selector: aSelector argumentList: anArray [
	^ self add: aString action: (MessageSend receiver: model selector: aSelector arguments: anArray)
]

{ #category : #construction }
PluggableMenuSpec >> add: aString target: anObject selector: aSelector argumentList: anArray [
	^ self add: aString translated action: (MessageSend receiver: anObject selector: aSelector arguments: anArray)
]

{ #category : #construction }
PluggableMenuSpec >> addList: aList [
	"Add the given items to this menu, where each item is a pair (<string> <actionSelector>).  If an element of the list is simply the symobl $-, add a line to the receiver. The optional third element of each entry, if present, provides balloon help."

	aList
		do: [ :tuple | 
			tuple == #-
				ifTrue: [ self addSeparator ]
				ifFalse: [ 
					| item |
					item := self
								add: tuple first
								target: model
								selector: tuple second
								argumentList: #().
					(tuple size > 2 and: [ tuple third notNil ])
						ifTrue: [ item help: tuple third ].
					(tuple size > 3 and: [ tuple fourth notNil ])
						ifTrue: [ item icon: (self theme iconNamed: tuple fourth) ] ] ]
]

{ #category : #construction }
PluggableMenuSpec >> addMenuItem [
	| item |
	item := self newMenuItem.
	self items add: item.
	^item
]

{ #category : #construction }
PluggableMenuSpec >> addSeparator [
	self items isEmpty
		ifTrue: [ ^ nil ].
	self items last separator: true
]

{ #category : #accessing }
PluggableMenuSpec >> items [
	^ items ifNil: [ items := OrderedCollection new ]
]

{ #category : #accessing }
PluggableMenuSpec >> label [
	^ label
]

{ #category : #accessing }
PluggableMenuSpec >> label: aString [
	label := aString.
]

{ #category : #accessing }
PluggableMenuSpec >> model [
	^ model
]

{ #category : #accessing }
PluggableMenuSpec >> model: anObject [ 
	model := anObject
]

{ #category : #construction }
PluggableMenuSpec >> newMenuItem [
	^ PluggableMenuItemSpec new
]
