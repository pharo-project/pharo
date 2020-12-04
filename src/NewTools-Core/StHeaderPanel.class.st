"
! SpHeaderPanel

A header panel is a presenter that provides a header and contains an arbitrary presenter. 

Through its property ==headerBar== (see *class:StHeaderBar*), it provides: a label, a shortcut <meta+panelNumber> and a small toolbar.

This presenter is used to homogenize the panels in tools.
"
Class {
	#name : #StHeaderPanel,
	#superclass : #SpPresenter,
	#instVars : [
		'headerBar',
		'presenter',
		'shortcut'
	],
	#category : #'NewTools-Core-Presenters'
}

{ #category : #initialization }
StHeaderPanel >> addWindowShortcutsTo: aWindowPresenter [

	shortcut ifNil: [ ^ self ].
	aWindowPresenter 
		bindKeyCombination: shortcut
		toAction: [ presenter takeKeyboardFocus ]
]

{ #category : #initialization }
StHeaderPanel >> initializePresenters [

	headerBar := self instantiate: StHeaderBar.
	
	layout := SpBoxLayout newVertical
		add: headerBar expand: false;
		yourself
]

{ #category : #'accessing header' }
StHeaderPanel >> label: aString [

	headerBar label: aString
]

{ #category : #accessing }
StHeaderPanel >> number: aNumber [

	self shortcut: (self shortcutForPanel: aNumber)
]

{ #category : #accessing }
StHeaderPanel >> presenter: aPresenter [

	presenter ifNotNil: [ layout remove: presenter ].
	presenter := aPresenter.
	layout add: presenter
]

{ #category : #'accessing header' }
StHeaderPanel >> shortcut: aShortcut [

	shortcut := aShortcut.
	headerBar shortcut: aShortcut
]

{ #category : #private }
StHeaderPanel >> shortcutForPanel: aNumber [
	"This is a convenience method to assign shortcuts to panels. 
	 panels will be different areas of a tool: a package panel in a browser, an inspector in 
	 the debugger."
	| char |
	
	char := aNumber asString first.
	^ char command mac 
		| char control win 
		| char control unix
]

{ #category : #'accessing header' }
StHeaderPanel >> toolbarActions: aCommandGroup [

	headerBar toolbarActions: aCommandGroup
]
