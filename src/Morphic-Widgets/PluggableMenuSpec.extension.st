Extension { #name : 'PluggableMenuSpec' }

{ #category : '*Morphic-Widgets' }
PluggableMenuSpec >> asMenuMorph [
	^ self asMenuMorphOfKind: self morphClass
]

{ #category : '*Morphic-Widgets' }
PluggableMenuSpec >> asMenuMorphOfKind: aMorphClass [
	| prior menu myitems |
	prior := parentMenu.
	parentMenu := menu := aMorphClass new.
	self label ifNotNil: [
		parentMenu buildTitle: [ :titleMorph |
			titleMorph bigTitle: self label
		]
	].
	myitems := self items.
	myitems do: [ :each | each asMenuItemMorphFrom: parentMenu isLast: each = myitems last ].
	parentMenu := prior.
	^ menu
]

{ #category : '*Morphic-Widgets' }
PluggableMenuSpec >> buildWith: aBuilder [
	^ self asMenuMorph
]

{ #category : '*Morphic-Widgets-Examples' }
PluggableMenuSpec class >> exampleWithOneSingleItem [
	"self exampleWithOneSingleItem"

	| s |
	s := (self withModel: nil) label: 'Hello'.
	s
		add: 'Print Hello'
		action: (MessageSend receiver: [ self traceNewLine: 'Hello' ] selector: #value).
	s asMenuMorph popUpInWorld
]

{ #category : '*Morphic-Widgets-Examples' }
PluggableMenuSpec class >> exampleWithTwoSimpleItems [
	"self exampleWithTwoSimpleItems"

	| s |
	s := (self withModel: Object) label: 'Hello'.
	s
		add: 'Print Hello'
		action: (MessageSend receiver: [ self traceNewLine: 'Hello' ] selector: #value).
	s
		add: 'Print Hello again'
		selector: #trace:
		argumentList: {('Hello again' , String cr)}.
	s asMenuMorph popUpInWorld
]

{ #category : '*Morphic-Widgets' }
PluggableMenuSpec >> morphClass [
	^ MenuMorph
]
