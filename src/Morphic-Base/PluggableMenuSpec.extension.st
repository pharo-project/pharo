Extension { #name : #PluggableMenuSpec }

{ #category : #'*Morphic-Base' }
PluggableMenuSpec >> asMenuMorph [
	^ self asMenuMorphOfKind: self morphClass
]

{ #category : #'*Morphic-Base' }
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

{ #category : #'*Morphic-Base' }
PluggableMenuSpec >> buildWith: aBuilder [
	^ self asMenuMorph
]

{ #category : #'*Morphic-Base-Examples' }
PluggableMenuSpec class >> exampleWithOneSingleItem [
	"self exampleWithOneSingleItem"

	| s |
	s := (self withModel: nil) label: 'Hello'.
	s
		add: 'Print Hello'
		action: (MessageSend receiver: [ self traceCr: 'Hello' ] selector: #value).
	s asMenuMorph popUpInWorld
]

{ #category : #'*Morphic-Base-Examples' }
PluggableMenuSpec class >> exampleWithSubMenu [
	"
	self exampleWithSubMenu
	"
	| s i sub |
	s := (self withModel: nil ) label: 'Tools'. 
	i := s add: 'Tools'.
	sub := (self withModel: nil ).
	sub add: 'System browser'  target: Smalltalk tools selector: #openClassBrowser argumentList: #().
	sub add: 'Workspace'  target: Smalltalk tools selector: #openWorkspace argumentList: #().
	i subMenu: sub.
	s asMenuMorph popUpInWorld
]

{ #category : #'*Morphic-Base-Examples' }
PluggableMenuSpec class >> exampleWithTwoSimpleItems [
	"self exampleWithTwoSimpleItems"

	| s |
	s := (self withModel: Object) label: 'Hello'.
	s
		add: 'Print Hello'
		action: (MessageSend receiver: [ self traceCr: 'Hello' ] selector: #value).
	s
		add: 'Print Hello again'
		selector: #trace:
		argumentList: {('Hello again' , String cr)}.
	s asMenuMorph popUpInWorld
]

{ #category : #'*Morphic-Base' }
PluggableMenuSpec >> morphClass [
	^ MenuMorph
]
