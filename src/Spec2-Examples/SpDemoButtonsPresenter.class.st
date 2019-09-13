"
This demo shows differenty styles of button elements.
"
Class {
	#name : #SpDemoButtonsPresenter,
	#superclass : #SpPresenter,
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
	#category : #'Spec2-Examples-Demo-Buttons'
}

{ #category : #specs }
SpDemoButtonsPresenter class >> defaultSpec [
	^ SpGridLayout new
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
SpDemoButtonsPresenter >> buttonDisabled [
	^ buttonDisabled
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonDisabled: anObject [
	buttonDisabled := anObject
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonHighlighted [
	^ buttonHighlighted
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonHighlighted: anObject [
	buttonHighlighted := anObject
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonNormal [
	^ buttonNormal
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonNormal: anObject [
	buttonNormal := anObject
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonWithColor [
	^ buttonWithColor
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonWithColor: anObject [
	buttonWithColor := anObject
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonWithDifferentFont [
	^ buttonWithDifferentFont
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonWithDifferentFont: anObject [
	buttonWithDifferentFont := anObject
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonWithHelp [
	^ buttonWithHelp
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonWithHelp: anObject [

	buttonWithHelp := anObject
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonWithIcon [
	^ buttonWithIcon
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonWithIcon: anObject [
	buttonWithIcon := anObject
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonWithIconOnly [
	^ buttonWithIconOnly
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonWithIconOnly: anObject [
	buttonWithIconOnly := anObject
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonWithMenu [
	^ buttonWithMenu
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonWithMenu: anObject [
	buttonWithMenu := anObject
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonWithShortcut [
	^ buttonWithShortcut
]

{ #category : #accessing }
SpDemoButtonsPresenter >> buttonWithShortcut: anObject [
	buttonWithShortcut := anObject
]

{ #category : #initialization }
SpDemoButtonsPresenter >> connectPresenters [

	buttonWithShortcut action: [ self inform: 'button with shortcut pressed' ]																																														
]

{ #category : #initialization }
SpDemoButtonsPresenter >> initializePresenters [
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
		contextMenu: self subMenu;
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
SpDemoButtonsPresenter >> subMenu [
	^ self newMenu
		addItem: [ :item | 
			item
				name: 'Open';
				icon: (self iconNamed: #openIcon);
				shortcut: $o meta;
				action: [ self inform: 'Open' ] ];
		addItem: [ :item | 
			item
				name: 'Save';
				icon: (self iconNamed: #smallSaveIcon);
				shortcut: $s meta;
				action: [ self inform: 'Save' ] ];
		addItem: [ :item | 
			item
				name: 'Print';
				shortcut: $p meta;
				icon: (self iconNamed: #smallPrintIcon);
				action: [ self inform: 'Print' ] ];
		addItem: [ :item | 
			item
				name: 'Kill';
				shortcut: $k meta;
				icon: (self iconNamed: #smallCancelIcon);
				action: [ self inform: 'Kill' ] ];
		yourself
]
