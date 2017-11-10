"
I'm a pluggable togglable button. 
I extend a pluggable button in a very simple way:  I keep two states, PRESSED, and UNPRESSED (in fact, is just a boolean).

I override a couple of methods, like #getModelState because in my context it does not have sense. 

See PluggableToggleButtonMorphExample to have some usage hints. 

Example:
========
(This is very simple and probably you want to use as is shown in the examples)

PluggableToggleButtonMorph new 
	icon: Smalltalk ui icons smallOkIcon;
	label: 'Test';
	actionBlock: [ :pressed | self inform: 'Status: ', (pressed asString) ];
	openInWindow
	

"
Class {
	#name : #PluggableToggleButtonMorph,
	#superclass : #PluggableButtonMorph,
	#instVars : [
		'pressed'
	],
	#category : #Morphic-Widgets-Basic-Buttons
}

{ #category : #accessing }
PluggableToggleButtonMorph >> bePressed [
	self pressed: true
]

{ #category : #accessing }
PluggableToggleButtonMorph >> icon [
	| originalIcon |
	originalIcon := super icon.
	originalIcon ifNil: [ ^ originalIcon ].
	^ self isPressed 
		ifTrue: [ (originalIcon asFormOfDepth: 32) dimmed ]
		ifFalse: [ originalIcon ]
]

{ #category : #initialization }
PluggableToggleButtonMorph >> initialize [ 
	pressed := false.
	super initialize
]

{ #category : #testing }
PluggableToggleButtonMorph >> isPressed [
	^ pressed
]

{ #category : #accessing }
PluggableToggleButtonMorph >> on: anObject getState: getStateSel action: actionSel label: labelSel icon: iconSel menu: menuSel [
	"Set up the pluggable parameters.
	Update label and state."

	super
		on: anObject
		getState: getStateSel
		action: actionSel
		label: labelSel
		icon: iconSel
		menu: menuSel.
	self pressed: super getModelState
]

{ #category : #accessing }
PluggableToggleButtonMorph >> performAction: event [
	"Inform the model that this button has been pressed. Sent by the controller when this button 
	 is pressed. If the button's actionSelector takes any arguments, they are obtained 
	 dynamically by sending the argumentSelector to the argumentsProvider"

	enabled ifFalse: [^self].
	askBeforeChanging ifTrue: [model okToChange ifFalse: [^ self]].
	
	"Toggle pressed"
	self pressed: self isPressed not.
	
	self actionBlock ifNotNil: [ ^ self actionBlock cull: self isPressed cull: event ].
	actionSelector ifNil: [ ^ self ].
	
	arguments := argumentsProvider 
		ifNotNil: [ argumentsProvider perform: argumentsSelector ]
		ifNil: [ #() ].
	model 
		perform: actionSelector 
		withArguments: (arguments copyWithFirst: self isPressed)
]

{ #category : #private }
PluggableToggleButtonMorph >> pressed: aBoolean [
	pressed := aBoolean.
	self update: (getLabelSelector ifNil: [ getIconSelector ])
]
