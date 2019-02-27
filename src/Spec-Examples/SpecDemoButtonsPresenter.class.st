"
This demo shows differenty styles of button elements.
"
Class {
	#name : #SpecDemoButtonsPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'buttonNormal',
		'buttonDisabled',
		'buttonWithIcon',
		'buttonWithIconOnly',
		'buttonHighlighted',
		'buttonWithColor',
		'buttonWithMenu',
		'buttonWithShortcut',
		'buttonWithHelp',
		'buttonWithDifferentFont'
	],
	#category : #'Spec-Examples-Demo-Buttons'
}

{ #category : #specs }
SpecDemoButtonsPresenter class >> defaultSpec [
	^ SpecGridLayout new
		beColumnHomogeneous;
		borderWidth: 0;
		add: #buttonNormal at: 1 @ 1;
		add: #buttonDisabled at: 2 @ 1;
		add: #buttonWithIcon at: 3 @ 1;
		add: #buttonWithIconOnly at: 4 @ 1;
		add: #buttonHighlighted at: 5 @ 1;
		add: #buttonWithColor at: 1 @ 2;
		add: #buttonWithMenu at: 2 @ 2;
		add: #buttonWithShortcut at: 3 @ 2;
		add: #buttonWithHelp at: 4 @ 2;
		add: #buttonWithDifferentFont at: 5 @ 2
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonDisabled [
	^ buttonDisabled
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonDisabled: anObject [
	buttonDisabled := anObject
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonHighlighted [
	^ buttonHighlighted
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonHighlighted: anObject [
	buttonHighlighted := anObject
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonNormal [
	^ buttonNormal
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonNormal: anObject [
	buttonNormal := anObject
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonWithColor [
	^ buttonWithColor
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonWithColor: anObject [
	buttonWithColor := anObject
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonWithDifferentFont [
	^ buttonWithDifferentFont
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonWithDifferentFont: anObject [
	buttonWithDifferentFont := anObject
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonWithHelp [
	^ buttonWithHelp
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonWithHelp: anObject [

	buttonWithHelp := anObject
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonWithIcon [
	^ buttonWithIcon
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonWithIcon: anObject [
	buttonWithIcon := anObject
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonWithIconOnly [
	^ buttonWithIconOnly
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonWithIconOnly: anObject [
	buttonWithIconOnly := anObject
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonWithMenu [
	^ buttonWithMenu
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonWithMenu: anObject [
	buttonWithMenu := anObject
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonWithShortcut [
	^ buttonWithShortcut
]

{ #category : #accessing }
SpecDemoButtonsPresenter >> buttonWithShortcut: anObject [
	buttonWithShortcut := anObject
]

{ #category : #initialization }
SpecDemoButtonsPresenter >> initializePresenter [

	buttonWithShortcut action: [ self inform: 'button with shortcut pressed' ]																																														
]

{ #category : #initialization }
SpecDemoButtonsPresenter >> initializeWidgets [
	buttonNormal := self newButton label: 'normal'.
	buttonDisabled := self newButton
		label: 'disabled';
		disable;
		yourself.
	buttonWithIcon := self newButton
		label: 'with icon';
		icon: ((self iconNamed: #pharo) scaledToSize: 16 @ 16);
		yourself.
	buttonWithIconOnly := self newButton
		icon: ((self iconNamed: #pharo) scaledToSize: 16 @ 16);
		help: 'This button has only icon, no label';
		yourself.
	buttonHighlighted := self newButton
		label: 'highlighted';
		state: true;
		yourself.
	buttonWithColor := self newButton
		label: 'red';
		color: Color red;
		yourself.
	buttonWithMenu := self newButton
		label: 'with menu';
		menu: self subMenu;
		yourself.
	buttonWithShortcut := self newButton
		shortcut: $o meta;
		label: 'with shortcut';
		yourself.
	buttonWithHelp := self newButton
		label: 'with help';
		help:
			'This button shows
a multiline help';
		yourself.

	buttonWithDifferentFont := self newButton
		label: 'different font';
		font: StandardFonts codeFont;
		yourself.

	self focusOrder
		add: buttonNormal;
		add: buttonDisabled;
		add: buttonWithIcon;
		add: buttonWithIconOnly;
		add: buttonHighlighted;
		add: buttonWithColor;
		add: buttonWithMenu;
		add: buttonWithShortcut;
		add: buttonWithHelp;
		add: buttonWithDifferentFont
]

{ #category : #initialization }
SpecDemoButtonsPresenter >> subMenu [ 

	^ MenuPresenter new
		addGroup: [ :group |
			group addItem: [ :item |
				item
					name: 'Open';
					icon: (self iconNamed: #openIcon);
					shortcut: $o meta;
					action: [ self inform: 'Open' ] ].
			group addItem: [ :item |
				item 
					name: 'Save';
					icon: (self iconNamed: #smallSaveIcon);
					shortcut: $s meta;
					action: [ self inform: 'Save' ] ].
			group addItem: [ :item |
				item 
					name: 'Print';
					shortcut: $p meta;
					icon: (self iconNamed: #smallPrintIcon);
					action: [ self inform: 'Print' ] ].
			group addItem: [ :item |
				item 
					name: 'Kill';
					shortcut: $k meta;
					icon: (self iconNamed: #smallCancelIcon);
					action: [ self inform: 'Kill' ] ] ]
]
