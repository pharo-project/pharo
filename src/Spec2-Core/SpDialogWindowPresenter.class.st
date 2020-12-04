"
A `SpDialogWindowPresenter` is a presenter used to describe a DialogWindow. 
Pay attention that it will not create a real modal in the sense that the user will keep the focus on other windows.

Usually instances of `SpDialogWindowPresenter` are created using the method `SpAbstractPresenter>>#openDialogWithSpec`.

If you want to make the user focus on your dialog only you should use `SpAbstractPresenter>>#openModalWithSpec` instead.

Canonical actions should be parameterized using the methods: `SpDialogWindowPresenter>>#cancelAction:` and `SpDialogWindowPresenter>>#okAction:`. 

## Usage 
Tipically, you do not instantiate directly a dialog, but you open a presenter as one.

```
SomePresenter>>#open
	^ self openDialogWithSpec
		okAction: [ self accept ];
		cancelAction: [ self cancel ];
		yourself
```
And to control how the dialog should behave, you define the method `initializeDialogWindow:` in your presenters.
```
SomePresenter>>#initializeDialogWindow: aDialogPresenter 
	super initializeDialogWindow: aDialogPresenter.
	aDialogPresenter
		initialExtent: 600@400;
		title: 'A title'	
```
"
Class {
	#name : #SpDialogWindowPresenter,
	#superclass : #SpWindowPresenter,
	#instVars : [
		'buttons',
		'okAction',
		'cancelAction',
		'cancelled',
		'defaultButton'
	],
	#category : #'Spec2-Core-Support'
}

{ #category : #specs }
SpDialogWindowPresenter class >> adapterName [

	^ #DialogWindowAdapter
]

{ #category : #private }
SpDialogWindowPresenter >> addButton: aButtonPresenter [

	buttons add: aButtonPresenter.
	^ aButtonPresenter
]

{ #category : #api }
SpDialogWindowPresenter >> addButton: aString do: aBlock [
	"Adds button logic to dialog.
	 This method will add a standard button to the button bar of the dialog, having `aString` 
	 as label and executing `aBlock` when pressed. 
	 `aBlock` receives this dialog as an optional argument.
	 This methods answers the added button presenter, to allow more specific/non standard  
	 definitions.
	 See `SpAbstractPresenter>>#initializeDialogWindow:` as an example of usage."
	
	^ self addButton: (SpButtonPresenter new 
		owner: self;
		label: aString;
		action: [ aBlock cull: self ];
		yourself)
]

{ #category : #private }
SpDialogWindowPresenter >> addDefaultButton: aButtonPresenter [
	"Adds button logic to dialog."
	
	defaultButton := self addButton: aButtonPresenter.
	defaultButton addStyle: 'default'.
	^ defaultButton
]

{ #category : #NOTUSED }
SpDialogWindowPresenter >> addDefaultButton: aString do: aBlock [
	"Adds button logic to dialog."
	
	defaultButton := self addButton: aString do: aBlock.
	defaultButton addStyle: 'default'.
	^ defaultButton
]

{ #category : #api }
SpDialogWindowPresenter >> beCancel [
	"Set the status of this dialog as 'cancelled'. 
	 See `SpDialogWindowPresenter>>#isCancelled`"

	cancelled := true
]

{ #category : #api }
SpDialogWindowPresenter >> beOk [
	"Set the status of this dialog as 'Ok'. 
	 See `SpDialogWindowPresenter>>#isOk`"

	cancelled := false
]

{ #category : #api }
SpDialogWindowPresenter >> buttons [ 
	"Answer an OrderedCollection that contains all defined buttons. 
	 Default action will always be the last one."
	
	self flag: #TODO. "Default action should be configurable, not the last action defined."
	
	^ buttons 
]

{ #category : #api }
SpDialogWindowPresenter >> cancelAction [
	"Answer the defined cancel action (the one that is executed when pressed 'cancel' button 
	 by default). In initialization phase, cancel action is defined as: 
	 ``` 
	self cancelAction: [ 
		cancelled := true.
		self close ]
	```
	(but it can be redefined by the user)"

	^ cancelAction
]

{ #category : #api }
SpDialogWindowPresenter >> cancelAction: aBlock [
	"Set the action to execute when 'cancel' button is pressed."

	cancelAction := aBlock
]

{ #category : #testing }
SpDialogWindowPresenter >> cancelled [
	"Answer true if dialog has been cancelled. 
	 This is a synonym of `SpDialogWindowPresenter>>#cancelled`"

	^ cancelled
]

{ #category : #private }
SpDialogWindowPresenter >> executeDefaultAction [

	defaultButton ifNil: [ ^ self ].
	defaultButton action cull: self
]

{ #category : #initialization }
SpDialogWindowPresenter >> initialize [

	super initialize.
	buttons := OrderedCollection new.
	cancelled := true.
	self initializeDefaultActions
]

{ #category : #initialization }
SpDialogWindowPresenter >> initializeDefaultActions [
	"default ok/cancel actions. 
	 this is just for backwards compatibility"
	self okAction: [ 
		cancelled := false.
		self close ].
	self cancelAction: [ 
		cancelled := true.
		self close ]
]

{ #category : #initialization }
SpDialogWindowPresenter >> initializeWindow [
		
	super initializeWindow.
	self presenter initializeDialogWindow: self
]

{ #category : #testing }
SpDialogWindowPresenter >> isCancelled [
	"Answer true if dialog has been cancelled."

	^ cancelled
]

{ #category : #testing }
SpDialogWindowPresenter >> isDialog [

	^ true
]

{ #category : #testing }
SpDialogWindowPresenter >> isOk [
	"Answer true if dialog has been accepted."

	^ self isCancelled not
]

{ #category : #api }
SpDialogWindowPresenter >> okAction [
	"Answer the defined ok action (the one that is executed when pressed 'ok' button 
	 by default). In initialization phase, ok action is defined as: 
	 ``` 
	self okAction: [ 
		cancelled := false.
		self close ]
	```
	(but it can be redefined by the user)"

	^ okAction
]

{ #category : #api }
SpDialogWindowPresenter >> okAction: aBlock [
	"Set the action to execute when 'ok' button is pressed."

	okAction := aBlock
]

{ #category : #'private-traversing' }
SpDialogWindowPresenter >> traverseInFocusOrderDo: aBlock excluding: excludes [

	super traverseInFocusOrderDo: aBlock excluding: excludes.
	self buttons do: [ :each |
		each traverseInFocusOrderDo: aBlock excluding: excludes ]	
]

{ #category : #api }
SpDialogWindowPresenter >> triggerCancelAction [
	"Trigger defined cancel action. 
	 See `SpDialogWindowPresenter>>#cancelAction:`"
	
	cancelAction ifNil: [ ^ self ].
	cancelAction cull: self
]

{ #category : #api }
SpDialogWindowPresenter >> triggerOkAction [
	"Trigger defined ok action. 
	 See `SpDialogWindowPresenter>>#okAction:`"

	okAction ifNil: [ ^ self ].
	okAction cull: self
]
