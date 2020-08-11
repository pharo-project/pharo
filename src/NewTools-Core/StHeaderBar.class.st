"
I am a generic header bar for tool component panels. 
I am used to add title, a toolbar and shortcut used to get to the panel.
"
Class {
	#name : #StHeaderBar,
	#superclass : #SpPresenter,
	#instVars : [
		'titleLabel',
		'toolbar',
		'closeButton',
		'shortcutLabel'
	],
	#category : #'NewTools-Core-Presenters'
}

{ #category : #specs }
StHeaderBar class >> defaultSpec [

	^ SpBoxLayout newHorizontal
		beNotHomogeneous;
		borderWidth: 3;
		spacing: 3;
		"add: #closeButton expand: false;"
		add: #titleLabel expand: false;
		add: #shortcutLabel expand: false;
		addLast: #toolbar expand: false;
		yourself
]

{ #category : #initialization }
StHeaderBar >> initializePresenters [
	self flag: 'Temp hack for headers: removing header styles'.
	closeButton := self newButton
		icon: (self application iconNamed: #windowClose);
		action: [  ];
		hide;
		yourself.
	titleLabel := self newLabel.
	shortcutLabel := self newLabel
		addStyle: 'shortcut';
		yourself.
	toolbar := self newToolbar.
	"toolbar displayMode: self application toolbarDisplayMode"
	toolbar beIcons	"is a mini toolbar, items should be... tiny :)"
]

{ #category : #accessing }
StHeaderBar >> label: aString [

	titleLabel label: aString
]

{ #category : #accessing }
StHeaderBar >> shortcut: aShortcut [ 

	self shortcutLabel: (KMShortcutPrinter toString: aShortcut)
]

{ #category : #private }
StHeaderBar >> shortcutLabel: aString [ 

	shortcutLabel label: aString
]

{ #category : #accessing }
StHeaderBar >> toolbarActions: aCommandGroup [

	toolbar removeAllItems.
	SpToolbarPresenterBuilder new 
		toolbarPresenter: toolbar;
		visit: aCommandGroup
]
