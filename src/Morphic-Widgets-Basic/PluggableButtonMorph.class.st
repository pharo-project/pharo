"
A PluggableButtonMorph is a combination of an indicator for a boolean value stored in its model and an action button. The action of a button is often, but not always, to toggle the boolean value that it shows. Its pluggable selectors are:

		getStateSelector		fetch a boolean value from the model
		actionSelector		invoke this button's action on the model
		getLabelSelector		fetch this button's lable from the model
		getMenuSelector		fetch a pop-up menu for this button from the model

Any of the above selectors can be nil, meaning that the model does not supply behavior for the given action, and the default behavior should be used. For example, if getStateSelector is nil, then this button shows the state of a read-only boolean that is always false.

The model informs its view(s) of changes by sending #changed: to itself with getStateSelector as a parameter. The view tells the model when the button is pressed by sending actionSelector.

If the actionSelector takes one or more arguments, then the following are relevant:
		arguments			A list of arguments to provide when the actionSelector is called.
		argumentsProvider	The object that is sent the argumentSelector to obtain arguments, if dynamic
		argumentsSelector	The message sent to the argumentProvider to obtain the arguments.

Options:
	askBeforeChanging		have model ask user before allowing a change that could lose edits
	triggerOnMouseDown	do this button's action on mouse down (vs. up) transition
	shortcutCharacter		a place to record an optional shortcut key

"
Class {
	#name : #PluggableButtonMorph,
	#superclass : #AlignmentMorph,
	#instVars : [
		'model',
		'label',
		'icon',
		'getIconSelector',
		'getStateSelector',
		'actionSelector',
		'font',
		'getLabelSelector',
		'getMenuSelector',
		'shortcutCharacter',
		'askBeforeChanging',
		'triggerOnMouseDown',
		'offColor',
		'onColor',
		'feedbackColor',
		'showSelectionFeedback',
		'allButtons',
		'arguments',
		'argumentsProvider',
		'argumentsSelector',
		'enabled',
		'actionBlock',
		'getColorSelector',
		'getEnabledSelector',
		'getFontSelector',
		'labelMorph',
		'iconMorph',
		'iconPosition',
		'lastState'
	],
	#category : #'Morphic-Widgets-Basic-Buttons'
}

{ #category : #shortcuts }
PluggableButtonMorph class >> buildPluggableButtonShortcutsOn: aBuilder [
	<keymap>
	(aBuilder shortcut: #action1)
		category: #PluggableButtonMorph
		default: Character space asKeyCombination | Character cr asKeyCombination
		do: [ :target :morph :event | morph performAction ].
]

{ #category : #examples }
PluggableButtonMorph class >> exampleButtonNoAction [
	<sampleInstance>
	<exampleWidget>

	^ PluggableButtonMorph
		newButtonFor: nil
		action: nil
		label: 'A Button'
		help: 'This is a button'
]

{ #category : #'instance creation' }
PluggableButtonMorph class >> newButtonFor: aModel action: actionSel label: stringOrText help: helpText [
	"Answer a new button."

	^self
		newButtonFor: aModel
		getState: nil
		action: actionSel
		arguments: nil
		getEnabled: nil 
		label: stringOrText
		help: helpText
]

{ #category : #'instance creation' }
PluggableButtonMorph class >> newButtonFor: aModel getState: stateSel action: actionSel arguments: args getEnabled: enabledSel label: label help: helpText [
	"Answer a new button."

	| b |
	b := self on: aModel getState: stateSel action: actionSel.
	b
		arguments: (args ifNil: [{b}]);
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		label: label ;
		getEnabledSelector: enabledSel;
		setBalloonText: helpText;
		extent: b minExtent;
		hResizing: #rigid;
		vResizing: #rigid.
	^b
]

{ #category : #'instance creation' }
PluggableButtonMorph class >> on: anObject [

	^ self on: anObject getState: #isOn action: #switch

]

{ #category : #'instance creation' }
PluggableButtonMorph class >> on: anObject getState: getStateSel action: actionSel [

	^ self new
		on: anObject
		getState: getStateSel
		action: actionSel
		label: nil
		icon: nil
		menu: nil

]

{ #category : #'instance creation' }
PluggableButtonMorph class >> on: anObject getState: getStateSel action: actionSel label: labelSel [

	^ self new
		on: anObject
		getState: getStateSel
		action: actionSel
		label: labelSel
		icon: nil
		menu: nil

]

{ #category : #'instance creation' }
PluggableButtonMorph class >> on: anObject getState: getStateSel action: actionSel label: labelSel icon: iconSel menu: menuSel [

	^ self new
		on: anObject
		getState: getStateSel
		action: actionSel
		label: labelSel
		icon: iconSel
		menu: menuSel

]

{ #category : #'instance creation' }
PluggableButtonMorph class >> on: anObject getState: getStateSel action: actionSel label: labelSel menu: menuSel [

	^ self new
		on: anObject
		getState: getStateSel
		action: actionSel
		label: labelSel
		icon: nil
		menu: menuSel

]

{ #category : #accessing }
PluggableButtonMorph >> action [
	"Answer the action selector."

	^self actionSelector
]

{ #category : #accessing }
PluggableButtonMorph >> action: aSymbol [ 
	"Set actionSelector to be the action defined by aSymbol."

	actionSelector := aSymbol.

]

{ #category : #accessing }
PluggableButtonMorph >> actionBlock [

	^ actionBlock 
]

{ #category : #accessing }
PluggableButtonMorph >> actionBlock: aBlock [
	"an action can be either specified by a block or a selector. If the block is set it takes priority over selector."
	
	actionBlock := aBlock
]

{ #category : #accessing }
PluggableButtonMorph >> actionSelector [
	"Answer the receiver's actionSelector"

	^ actionSelector
]

{ #category : #accessing }
PluggableButtonMorph >> actionSelector: aSymbol [ 
	"Set actionSelector to be the action defined by aSymbol.
	SimpleButtonMorph cross-compatibility"

	actionSelector := aSymbol.

]

{ #category : #'event handling' }
PluggableButtonMorph >> addStandardHaloMenuItemsTo: aMenu hand: aHandMorph [

	aMenu add: 'browse action' selector: #browseAction.
	aMenu addLine.
	super addStandardHaloMenuItemsTo: aMenu hand: aHandMorph.
]

{ #category : #updating }
PluggableButtonMorph >> adoptColor: aColor [
	"Go through paneColorChanged instead."
	
	self paneColorChanged
]

{ #category : #updating }
PluggableButtonMorph >> adoptPaneColor: aColor [

	super adoptPaneColor: aColor.
	aColor ifNil: [^self].
	self adoptColor: self colorToUse
]

{ #category : #accessing }
PluggableButtonMorph >> arguments [
	"Answer the static arguments.
	SimpleButtonMorph cross-compatibility."

	^arguments
]

{ #category : #arguments }
PluggableButtonMorph >> arguments: args [
	"If the receiver takes argument(s) that are static, they can be filled by calling this.  If its argument(s) are to be dynamically determined, then use an argumentProvider and argumentSelector instead"

	arguments := args
]

{ #category : #arguments }
PluggableButtonMorph >> argumentsProvider: anObject argumentsSelector: aSelector [
	"Set the argument provider and selector"

	argumentsProvider := anObject.
	argumentsSelector := aSelector
]

{ #category : #accessing }
PluggableButtonMorph >> askBeforeChanging [

	^ askBeforeChanging

]

{ #category : #accessing }
PluggableButtonMorph >> askBeforeChanging: aBoolean [
	"If this preference is turned on, then give the model an opportunity to ask the user before accepting a change that might cause unaccepted edits to be lost."

	askBeforeChanging := aBoolean.

]

{ #category : #accessing }
PluggableButtonMorph >> availableBorderStyles [
	"Return the selectors of the the available border styles for each state.
	Must match the order of interactionStates."
	
	^#(normalBorderStyle mouseOverBorderStyle pressedBorderStyle disabledBorderStyle
		selectedBorderStyle selectedPressedBorderStyle selectedMouseOverBorderStyle selectedDisabledBorderStyle)
]

{ #category : #accessing }
PluggableButtonMorph >> availableFillStyles [
	"Return the selectors of the the available fill styles for each state.
	Must match the order of interactionStates."
	
	^#(normalFillStyle mouseOverFillStyle pressedFillStyle disabledFillStyle
		selectedFillStyle selectedPressedFillStyle selectedMouseOverFillStyle selectedDisabledFillStyle)
]

{ #category : #accessing }
PluggableButtonMorph >> beIconBottom [
	"show icon top to label"
	iconPosition := #bottom.
	self update: getIconSelector
]

{ #category : #accessing }
PluggableButtonMorph >> beIconLeft [
	"show icon left to label"
	iconPosition := #left.
	self update: getIconSelector
]

{ #category : #accessing }
PluggableButtonMorph >> beIconRight [
	"show icon right to label"
	iconPosition := #right.
	self update: getIconSelector
]

{ #category : #accessing }
PluggableButtonMorph >> beIconTop [
	"show icon top to label"
	iconPosition := #top.
	self update: getIconSelector
]

{ #category : #accessing }
PluggableButtonMorph >> borderStyleToUse [
	"Return the borderStyle to use for the receiver."

	^ self perform: (self availableBorderStyles at: (self interactionStates indexOf: self interactionState))
]

{ #category : #private }
PluggableButtonMorph >> browseAction [

	| classDefiningAction |
	classDefiningAction := self model class whichClassIncludesSelector: self actionSelector.
	Smalltalk tools browser openOnClass: classDefiningAction selector: self actionSelector.
]

{ #category : #updating }
PluggableButtonMorph >> changed [
	"Update the fillStyle here."

	|lc pc bs|
	self assureExtension.
	extension borderStyle: (bs := self borderStyleToUse).
	borderColor := bs style.
	borderWidth := bs width.
	extension fillStyle: self fillStyleToUse.
	self layoutInset: (self theme buttonLabelInsetFor: self).
	self privateColor: self fillStyle asColor.
	(self labelMorph respondsTo: #interactionState:)
		ifTrue: [self labelMorph interactionState: self interactionState]
		ifFalse: [(self labelMorph isNil
			or: [label isMorph]) ifFalse: [
				pc := self normalColor.
				lc := self enabled
					ifTrue: [pc contrastingBlackAndWhiteColor]
					ifFalse: [pc contrastingBlackAndWhiteColor muchDarker].
				self labelMorph color: lc]].
	super changed
]

{ #category : #accessing }
PluggableButtonMorph >> color: aColor [
	"Check to avoid repeats of the same color."

	aColor ifNil: [ ^ self ].
	(aColor = self color and: [ self getModelState = self lastState ]) ifTrue: [ ^ self ].
	
	super color: aColor
]

{ #category : #accessing }
PluggableButtonMorph >> colorToUse [
	"Answer the color we should use."

	|c|
	c := self getModelState
		ifTrue: [onColor
					ifNil: [self paneColor]
					ifNotNil: [onColor isTransparent
								ifTrue: [self paneColor]
								ifFalse: [onColor]]]
		ifFalse: [offColor
					ifNil: [self paneColor]
					ifNotNil: [offColor isTransparent
								ifTrue: [self paneColor]
								ifFalse: [offColor]]].
	^c
]

{ #category : #accessing }
PluggableButtonMorph >> contentHolder [
	"Answer the alignment morph for extra control."

	^self submorphs first
]

{ #category : #accessing }
PluggableButtonMorph >> cornerStyle: aSymbol [
	"Adjust the layout inset."

	super cornerStyle: aSymbol.
	self layoutInset: (self theme buttonLabelInsetFor: self)
]

{ #category : #initialization }
PluggableButtonMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ self theme borderWidth
]

{ #category : #initialization }
PluggableButtonMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ self containingWindow ifNil: [Color lightGreen] ifNotNil: [:w | w defaultBackgroundColor]
]

{ #category : #accessing }
PluggableButtonMorph >> disable [
	"Disable the button."

	self enabled: false
]

{ #category : #accessing }
PluggableButtonMorph >> disabledBorderStyle [
	"Return the disabled borderStyle of the receiver."
	
	^self theme buttonDisabledBorderStyleFor: self
]

{ #category : #accessing }
PluggableButtonMorph >> disabledFillStyle [
	"Return the disabled fillStyle of the receiver."
	
	^self theme buttonDisabledFillStyleFor: self
]

{ #category : #drawing }
PluggableButtonMorph >> drawSubmorphsOn: aCanvas [ 
	"Display submorphs back to front.
	Draw the focus here since we are using inset bounds
	for the focus rectangle."

	super drawSubmorphsOn: aCanvas.
	self hasKeyboardFocus ifTrue: [self drawKeyboardFocusOn: aCanvas]
]

{ #category : #accessing }
PluggableButtonMorph >> enable [
	"Enable the button."

	self enabled: true
]

{ #category : #accessing }
PluggableButtonMorph >> enabled [
	^ enabled ifNil: [enabled := true]
]

{ #category : #accessing }
PluggableButtonMorph >> enabled: aBoolean [
	"Set the enabled state of the receiver."
	
	enabled = aBoolean ifTrue: [^self].
	enabled := aBoolean.
	self updateLabelEnablement.
	self changed
]

{ #category : #accessing }
PluggableButtonMorph >> feedbackColor: aColor [
	"Set the color of this button's selection feedback border."

	feedbackColor := aColor.
	self changed.

]

{ #category : #accessing }
PluggableButtonMorph >> fillStyleToUse [
	"Return the fillStyle to use for the receiver."
	
	^self perform: (self availableFillStyles at: (
			self interactionStates indexOf: self interactionState))

]

{ #category : #accessing }
PluggableButtonMorph >> focusBounds [
	"Answer the bounds for drawing the focus indication."

	^self theme buttonFocusBoundsFor: self
]

{ #category : #accessing }
PluggableButtonMorph >> focusColor [
	"Answer the keyboard focus indication color."

	^self color contrastingBlackAndWhiteColor
]

{ #category : #accessing }
PluggableButtonMorph >> focusIndicatorCornerRadius [
	"Answer the corner radius preferred for the focus indicator
	for the receiver for themes that support this."

	^self theme buttonFocusIndicatorCornerRadiusFor: self 
]

{ #category : #accessing }
PluggableButtonMorph >> font [
	^ font
]

{ #category : #accessing }
PluggableButtonMorph >> font: anObject [
	font := anObject.
	self update: getLabelSelector.
]

{ #category : #accessing }
PluggableButtonMorph >> getColorSelector [
	^getColorSelector
]

{ #category : #accessing }
PluggableButtonMorph >> getColorSelector: aSymbol [
	getColorSelector := aSymbol.
	self update: getColorSelector.
]

{ #category : #accessing }
PluggableButtonMorph >> getEnabledSelector [
	^getEnabledSelector
]

{ #category : #accessing }
PluggableButtonMorph >> getEnabledSelector: aSymbol [
	getEnabledSelector := aSymbol.
	self update: aSymbol.
]

{ #category : #accessing }
PluggableButtonMorph >> getFontSelector [
	^ getFontSelector
]

{ #category : #accessing }
PluggableButtonMorph >> getFontSelector: anObject [
	getFontSelector := anObject.
	self update: getFontSelector.
]

{ #category : #private }
PluggableButtonMorph >> getMenu: shiftPressed [

	"Answer the menu for this button, supplying an empty menu to be filled in. If the menu selector takes an extra argument, pass in the current state of the shift key."

	| menu |

	getMenuSelector ifNil: [ ^ nil ].
	menu := self theme newMenuIn: self for: model.
	getMenuSelector numArgs = 1
		ifTrue: [ ^ model perform: getMenuSelector with: menu ].
	getMenuSelector numArgs = 2
		ifTrue: [ ^ model perform: getMenuSelector with: menu with: shiftPressed ].
	^ self error: 'The getMenuSelector must be a 1- or 2-keyword symbol'
]

{ #category : #accessing }
PluggableButtonMorph >> getMenuSelector: aSymbol [
	"Set the menu selector."

	getMenuSelector := aSymbol
]

{ #category : #accessing }
PluggableButtonMorph >> getModelState [
	"Answer the result of sending the receiver's model the getStateSelector message.
	If the selector expects arguments then supply as for the actionSelector."

	model ifNil: [^ false].
	^getStateSelector ifNil: [false]
		ifNotNil: [getStateSelector numArgs = 0
					ifTrue: [model perform: getStateSelector]
					ifFalse: [argumentsProvider ifNotNil: [
								arguments := argumentsProvider perform: argumentsSelector].
							model perform: getStateSelector withEnoughArguments: arguments]]
]

{ #category : #'event handling' }
PluggableButtonMorph >> handlesKeyboard: evt [
	"Answer true, we'll handle spacebar for pressing plus the usual
	tab navigation."
	
	^true
]

{ #category : #'event handling' }
PluggableButtonMorph >> handlesMouseDown: evt [

	^ true

]

{ #category : #'event handling' }
PluggableButtonMorph >> handlesMouseOver: evt [

	^ true
]

{ #category : #'event handling' }
PluggableButtonMorph >> handlesMouseOverDragging: evt [

	^ true
]

{ #category : #balloon }
PluggableButtonMorph >> helpText [ 
	^ self balloonText
]

{ #category : #balloon }
PluggableButtonMorph >> helpText: aString [

	self setBalloonText: aString
]

{ #category : #accessing }
PluggableButtonMorph >> icon [
	^ icon
]

{ #category : #accessing }
PluggableButtonMorph >> icon: anIcon [
	icon := anIcon
]

{ #category : #accessing }
PluggableButtonMorph >> iconPosition [
	"Position of the icon. 
	  #left - Icon will be left to the label
	  #right - Icon will be right to the label 
	  #top - Icon will be top to the label"
	^ iconPosition ifNil: [ iconPosition := #left ]
]

{ #category : #drawing }
PluggableButtonMorph >> indicateModalChild [
	"Flash the button border."
	
	|fs c w d|
	fs := self fillStyle.
	c := self color alphaMixed: 0.5 with: Color black.
	w := self world.
	d := 0.
	self assureExtension.
	2 timesRepeat: [
		(Delay forDuration: d milliSeconds) wait.
		d := 200.
		extension fillStyle: c.
		self privateColor: c.
		self invalidRect: self bounds.
		w ifNotNil: [w displayWorldSafely].
		(Delay forDuration: d milliSeconds) wait.
		self fillStyle: fs.
		w ifNotNil: [w displayWorldSafely].
		self invalidRect: self bounds]

]

{ #category : #initialization }
PluggableButtonMorph >> initialize [
	"Initialize the state of the receiver."

	super initialize.
	self
		rubberBandCells: false;
		listDirection: #topToBottom;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		wrapCentering: #center;
		cellPositioning: #center.
	enabled := true.
	self initializeLabelMorph.	"this is a safe guard for enabled: among others."
	askBeforeChanging := false.
	triggerOnMouseDown := false.
	showSelectionFeedback := false.
	arguments := #().
	self
		layoutInset: (self theme buttonLabelInsetFor: self);
		extent: 20 @ 15;
		lastState: false;
		cornerStyle: (self theme buttonCornerStyleIn: nil);
		borderStyle: (self theme buttonNormalBorderStyleFor: self).
		
	self clipSubmorphs: true
]

{ #category : #initialization }
PluggableButtonMorph >> initializeLabelMorph [
	labelMorph := LabelMorph new
		extent: 0 @ 0;
		yourself	"this is a safe guard for enabled: among others."
]

{ #category : #keymapping }
PluggableButtonMorph >> initializeShortcuts: aKMDispatcher [
	super initializeShortcuts: aKMDispatcher.
	aKMDispatcher attachCategory: #MorphFocusNavigation.
	aKMDispatcher attachCategory: #PluggableButtonMorph
]

{ #category : #accessing }
PluggableButtonMorph >> interactionState [
	"Return the state that the receiver is in, #pressed, #normal etc.."
	
	|state mo|
	state := self getModelState.
	mo := (self valueOfProperty: #mouseEntered) == true.
	
	^(self enabled ifNil: [true])
		ifTrue: [showSelectionFeedback
			ifTrue: [state
				ifTrue: [#selectedPressed]
				ifFalse: [#pressed]]
			ifFalse: [mo
				ifTrue: [state
					ifTrue: [#selectedMouseOver]
					ifFalse: [#mouseOver]]
				ifFalse: [state
					ifTrue: [#selected]
					ifFalse: [#normal]]]]
		ifFalse: [state
			ifTrue: [#selectedDisabled]
			ifFalse: [#disabled]]
]

{ #category : #accessing }
PluggableButtonMorph >> interactionStates [
	"Return all the states that the receiver may be in at any given moment."
	
	^#(normal mouseOver pressed disabled
		selected selectedPressed selectedMouseOver selectedDisabled)
]

{ #category : #private }
PluggableButtonMorph >> invokeMenu: evt [
	"Invoke my menu in response to the given event."
	| menu |
	menu := self getMenu: evt shiftPressed.
	menu ifNotNil: [menu popUpEvent: evt in: self world]
]

{ #category : #accessing }
PluggableButtonMorph >> isDefault [
	"Answer whether the button is considered to be a default one."

	^self valueOfProperty: #isDefault ifAbsent: [false] 
]

{ #category : #accessing }
PluggableButtonMorph >> isDefault: aBoolean [
	"Set whether the button is to be considered default."

	aBoolean
		ifTrue: [self setProperty: #isDefault toValue: true]
		ifFalse: [self removeProperty: #isDefault].
	self changed
]

{ #category : #keyboard }
PluggableButtonMorph >> keyStroke: event [ 
	"Process spacebar for action and tab keys for navigation."
	
	(self navigationKey: event) ifTrue: [^self].
	(event keyCharacter = Character space or: [event keyCharacter = Character cr])
		ifTrue: [self performAction]
]

{ #category : #keyboard }
PluggableButtonMorph >> keyboardFocusChange: aBoolean [
	"The message is sent to a morph when its keyboard focus changes.
	Update for focus feedback."
	super keyboardFocusChange: aBoolean.
	self focusChanged
]

{ #category : #accessing }
PluggableButtonMorph >> label [
	"Answer the DisplayObject used as this button's label."

	^ label

]

{ #category : #accessing }
PluggableButtonMorph >> label: aStringOrTextOrMorph [
	"Label this button with the given string or morph."
	font ifNil: [
		self label: aStringOrTextOrMorph font: self theme buttonFont]
	ifNotNil: [ self label: aStringOrTextOrMorph font: font ]
]

{ #category : #accessing }
PluggableButtonMorph >> label: aStringOrTextOrMorph font: aFont [
	"Label this button with the given string or morph."
	| containerMorph |

	self removeAllMorphs.
	"nest label in a row for centering"
	containerMorph := AlignmentMorph new
		borderWidth: 0;
		layoutInset: 0;
		cellInset: 2;
		color: Color transparent;
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		wrapCentering: #center;
		listCentering: #center;
		cellPositioning: #center;
		yourself.
		
	(self iconPosition == #top or: [ self iconPosition == #bottom ]) 
		ifTrue: [ containerMorph setAsColumn ]
		ifFalse: [ containerMorph setAsRow ].

	label := aStringOrTextOrMorph.	
	self label ifNotNil: [ 
		labelMorph := self label isMorph
			ifTrue: [ self label ]
			ifFalse: [ self newLabel: aFont ].
		containerMorph addMorph: labelMorph ].

	self icon ifNotNil: [
		iconMorph := self icon asMorph.
		((self iconPosition == #right) or: [ self iconPosition == #bottom ])
			ifTrue: [ containerMorph addMorphBack: iconMorph ]
			ifFalse: [ containerMorph addMorphFront: iconMorph ] ].
	
	self addMorph: containerMorph.
	self updateLabelEnablement
]

{ #category : #accessing }
PluggableButtonMorph >> labelMorph [
	"Answer the actual label morph."

	^ labelMorph
]

{ #category : #accessing }
PluggableButtonMorph >> lastState [
	^ lastState
]

{ #category : #accessing }
PluggableButtonMorph >> lastState: anObject [
	lastState := anObject
]

{ #category : #accessing }
PluggableButtonMorph >> minHeight [
	"Consult the theme also."

	^super minHeight max: self theme buttonMinHeight
]

{ #category : #accessing }
PluggableButtonMorph >> minWidth [
	"Consult the theme also."

	^super minWidth max: self theme buttonMinWidth
]

{ #category : #accessing }
PluggableButtonMorph >> model [
	"Answer the receiver's model."
	
	^model
]

{ #category : #accessing }
PluggableButtonMorph >> model: anObject [
	"Set my model and make me me a dependent of the given object."

	model ifNotNil: [model removeDependent: self].
	anObject ifNotNil: [anObject addDependent: self].
	model := anObject.

]

{ #category : #'event handling' }
PluggableButtonMorph >> mouseDown: evt [
	"Details: If this button is triggered on mouse down or the event is the menu gesture, handle it immediately. Otherwise, make a list of buttons (including the receiver) for mouseMove feedback. This allows a simple radio-button effect among the button submorphs of a given morph."

	self enabled ifFalse: [^self].
	allButtons := nil.
	evt yellowButtonPressed ifTrue: [^ self invokeMenu: evt].
	self wantsKeyboardFocusOnMouseDown
		ifTrue: [self takeKeyboardFocus].
	triggerOnMouseDown
		ifTrue: [ self performAction: evt ]
		ifFalse: [
			allButtons := owner submorphs select: [:m | m class = self class].
			self updateFeedbackForEvt: evt].

]

{ #category : #'event handling' }
PluggableButtonMorph >> mouseEnter: evt [
	"Update the appearance."

	self setProperty: #mouseEntered toValue: true.
	self changed.
	"0.09375 is exact in floating point so no cumulative rounding error will occur"
	self color: (self color adjustBrightness: -0.09375).
	super mouseEnter: evt
]

{ #category : #'event handling' }
PluggableButtonMorph >> mouseLeave: evt [
	"Update the appearance."

	self setProperty: #mouseEntered toValue: false.
	self changed.
	"0.09375 is exact in floating point so no cumulative rounding error will occur"
	self color: (self color adjustBrightness: 0.09375).
	super mouseLeave: evt
]

{ #category : #'event handling' }
PluggableButtonMorph >> mouseLeaveDragging: evt [

	self mouseLeave: evt
]

{ #category : #'event handling' }
PluggableButtonMorph >> mouseMove: evt [

	allButtons ifNil: [^ self].
	allButtons do: [:m | m updateFeedbackForEvt: evt].

]

{ #category : #style }
PluggableButtonMorph >> mouseOverBorderStyle [
	"Return the mouse over borderStyle of the receiver."
	
	^self theme buttonMouseOverBorderStyleFor: self
]

{ #category : #style }
PluggableButtonMorph >> mouseOverFillStyle [
	"Return the mouse over fillStyle of the receiver."
	
	^self theme buttonMouseOverFillStyleFor: self
]

{ #category : #'event handling' }
PluggableButtonMorph >> mouseUp: evt [
	"Perform the button action if the mouse pointer is in a button in the group.
	Optimised feedback updates."
	
	|all|
	all := allButtons copy.
	all ifNotNil: [all do: [:m |
		m showSelectionFeedback ifTrue: [
			m showSelectionFeedback: false; changed; layoutChanged]]].
	all ifNil: [^ self].
	allButtons := nil.
	all do: [:m |
		(m containsPoint: evt cursorPoint) ifTrue: [m enabled ifTrue: [ m performAction: evt ]]].
	self showSelectionFeedback ifTrue: [self changed]

]

{ #category : #accessing }
PluggableButtonMorph >> newLabel [
	"Answer a new label for the receiver."
	|lbl|
	lbl := self theme buttonLabelFor: self.
	font ifNotNil: [lbl font: font.
		lbl extent: lbl optimalExtent ].
	^lbl
]

{ #category : #accessing }
PluggableButtonMorph >> newLabel: aFont [
	"Answer a new label for the receiver with the given font."

	^self newLabel
		font: aFont
]

{ #category : #style }
PluggableButtonMorph >> normalBorderStyle [
	"Return the normal borderStyle of the receiver."
	
	^self theme buttonNormalBorderStyleFor: self
]

{ #category : #accessing }
PluggableButtonMorph >> normalColor [
	"Return the normal colour for the receiver."
	
	^self theme buttonColorFor: self
]

{ #category : #style }
PluggableButtonMorph >> normalFillStyle [
	"Return the normal fillStyle of the receiver."
	
	^self theme buttonNormalFillStyleFor: self
]

{ #category : #accessing }
PluggableButtonMorph >> offColor [
	^ offColor

]

{ #category : #accessing }
PluggableButtonMorph >> offColor: colorWhenOff [
	"Set the fill colors to be used when this button is off."

	self onColor: onColor offColor: colorWhenOff

]

{ #category : #accessing }
PluggableButtonMorph >> on: anObject getState: getStateSel action: actionSel label: labelSel icon: iconSel menu: menuSel [
	"Set up the pluggable parameters.
	Update label and state."
	
	self model: anObject.
	getStateSelector := getStateSel.
	actionSelector := actionSel.
	getLabelSelector := labelSel.
	getIconSelector := iconSel.
	getMenuSelector := menuSel.
	self
		update: (labelSel ifNil: [ iconSel ]);
		update: getStateSel

]

{ #category : #accessing }
PluggableButtonMorph >> on: anObject getState: getStateSel action: actionSel label: labelSel menu: menuSel [
	"This method is for backward compatibility."
	self 
		on: anObject 
		getState: getStateSel 
		action: actionSel 
		label: labelSel 
		icon: nil 
		menu: menuSel
]

{ #category : #accessing }
PluggableButtonMorph >> onColor [
	"Answer the on color."
	
	^onColor

]

{ #category : #accessing }
PluggableButtonMorph >> onColor: colorWhenOn offColor: colorWhenOff [
	"Set the fill colors to be used when this button is on/off."

	onColor := colorWhenOn.
	offColor := colorWhenOff.
	self update: #onOffColor.
	self update: getStateSelector.



]

{ #category : #updating }
PluggableButtonMorph >> paneColorChanged [
	"Use changed to update the appearance."

	self changed
]

{ #category : #accessing }
PluggableButtonMorph >> performAction [
	"backward compatibility"
	
	self performAction: nil
]

{ #category : #accessing }
PluggableButtonMorph >> performAction: event [
	"Inform the model that this button has been pressed. Sent by the controller when this button is pressed. If the button's actionSelector takes any arguments, they are obtained dynamically by sending the argumentSelector to the argumentsProvider"

	enabled ifFalse: [^self].
	askBeforeChanging ifTrue: [model okToChange ifFalse: [^ self]].
	self actionBlock ifNotNil: [ ^ self actionBlock cull: event ].
	actionSelector ifNotNil:
		[actionSelector numArgs = 0
			ifTrue: [model perform: actionSelector]
			ifFalse: 
				[argumentsProvider ifNotNil:
					[arguments := argumentsProvider perform: argumentsSelector].
					model perform: actionSelector withArguments: arguments]]
]

{ #category : #style }
PluggableButtonMorph >> pressedBorderStyle [
	"Return the pressed borderStyle of the receiver."
	
	^self theme buttonPressedBorderStyleFor: self
]

{ #category : #style }
PluggableButtonMorph >> pressedFillStyle [
	"Return the pressed fillStyle of the receiver."
	
	^self theme buttonPressedFillStyleFor: self
]

{ #category : #accessing }
PluggableButtonMorph >> roundedCorners: anArray [
	"Adjust the layout inset if necessary."

	super roundedCorners: anArray.
	self layoutInset: (self theme buttonLabelInsetFor: self)
]

{ #category : #accessing }
PluggableButtonMorph >> selectedBorderStyle [
	"Return the selected borderStyle of the receiver."
	
	^self theme buttonSelectedBorderStyleFor: self
]

{ #category : #accessing }
PluggableButtonMorph >> selectedDisabledBorderStyle [
	"Return the selected disabled borderStyle of the receiver."
	
	^self theme buttonSelectedDisabledBorderStyleFor: self
]

{ #category : #style }
PluggableButtonMorph >> selectedDisabledFillStyle [
	"Return the selected disabled fillStyle of the receiver."
	
	^self theme buttonSelectedDisabledFillStyleFor: self
]

{ #category : #style }
PluggableButtonMorph >> selectedFillStyle [
	"Return the selected fillStyle of the receiver."
	
	^self theme buttonSelectedFillStyleFor: self
]

{ #category : #style }
PluggableButtonMorph >> selectedMouseOverBorderStyle [
	"Return the selected mouse over borderStyle of the receiver."
	
	^self theme buttonSelectedMouseOverBorderStyleFor: self
]

{ #category : #style }
PluggableButtonMorph >> selectedMouseOverFillStyle [
	"Return the selected mouse over fillStyle of the receiver."
	
	^self theme buttonSelectedMouseOverFillStyleFor: self
]

{ #category : #style }
PluggableButtonMorph >> selectedPressedBorderStyle [
	"Return the selected pressed borderStyle of the receiver."
	
	^self theme buttonSelectedPressedBorderStyleFor: self
]

{ #category : #style }
PluggableButtonMorph >> selectedPressedFillStyle [
	"Return the selected pressed fillStyle of the receiver."
	
	^self theme buttonSelectedPressedFillStyleFor: self
]

{ #category : #accessing }
PluggableButtonMorph >> shortcutCharacter [
	"Return the Character to be used as a shortcut to turn on this switch, or nil if this switch doesn't have a keyboard shortcut."

	^ shortcutCharacter

]

{ #category : #accessing }
PluggableButtonMorph >> shortcutCharacter: aCharacter [ 
	"Set the character to be used as a keyboard shortcut for turning on this switch."

	shortcutCharacter := aCharacter.

]

{ #category : #accessing }
PluggableButtonMorph >> showSelectionFeedback [
	"Answer whether the feedback should be shown for being pressed."

	^showSelectionFeedback
]

{ #category : #accessing }
PluggableButtonMorph >> showSelectionFeedback: aBoolean [
	"Set the feedback."

	showSelectionFeedback := aBoolean
]

{ #category : #keyboard }
PluggableButtonMorph >> takesKeyboardFocus [
	"Answer whether the receiver can normally take keyboard focus."

	^true
]

{ #category : #accessing }
PluggableButtonMorph >> target [
	^model
]

{ #category : #updating }
PluggableButtonMorph >> themeChanged [
	"Set the border style to thin gray in the case of going to StandardSqueak."

	self
		layoutInset: (self theme buttonLabelInsetFor: self);
		cornerStyle: (self theme buttonCornerStyleIn: self window);
		borderStyle: (self theme buttonNormalBorderStyleFor: self).
		
	(self labelMorph isNil or: [self label isMorph ]) ifFalse: [
		self label: self label font: self labelMorph font.
		self labelMorph color: self theme textColor
	].

	super themeChanged
]

{ #category : #accessing }
PluggableButtonMorph >> triggerOnMouseDown [

	^ triggerOnMouseDown

]

{ #category : #accessing }
PluggableButtonMorph >> triggerOnMouseDown: aBoolean [
	"If this preference is turned on, then trigger my action immediately when the mouse goes down."

	triggerOnMouseDown := aBoolean.

]

{ #category : #updating }
PluggableButtonMorph >> update: aParameter [ 
	|state|
	
	aParameter ifNil: [^self].
	"I associate label and icon because both are part of a 'complex' label... and update one 
	 means update the other"
	((aParameter == getLabelSelector) or: [ (aParameter == getIconSelector) ]) ifTrue: [
		getIconSelector ifNotNil: [ self icon: (model perform: getIconSelector) ].
		self label: (getLabelSelector ifNotNil: [ model perform: getLabelSelector])].
	aParameter == getFontSelector ifTrue: [ self font: (model perform: getFontSelector) ].
	state := self getModelState.
	(state ~= (self lastState) 
		or: [ getStateSelector isNil and: [aParameter == #onOffColor]])
		ifTrue: [
			self color: self colorToUse.
			self lastState: state].
	aParameter == getEnabledSelector ifTrue: [^self enabled: (model perform: getEnabledSelector)].
	getColorSelector ifNotNil: [ | cc |
		color = (cc := model perform: getColorSelector) 
			ifFalse: [
				self privateColor: cc.
				self onColor: color offColor: color.
				self changed ]].
		
	aParameter isArray ifFalse: [ ^ self ].
	aParameter size == 2 ifFalse: [ ^ self ].
	
	aParameter first = #askBeforeChanging: ifTrue: [ self askBeforeChanging: aParameter second ]
]

{ #category : #updating }
PluggableButtonMorph >> updateFeedbackForEvt: evt [

	| newState |
	newState := self containsPoint: evt cursorPoint.
	newState = showSelectionFeedback ifFalse: [
		self showSelectionFeedback: newState.
		self changed; layoutChanged].

]

{ #category : #updating }
PluggableButtonMorph >> updateLabelEnablement [
	"Set the enabled state of the label if possible."
	
	self labelMorph enabled: self enabled
]

{ #category : #copying }
PluggableButtonMorph >> veryDeepFixupWith: deepCopier [
	"If fields were weakly copied, fix them here.  If they were in the tree being copied, fix them up, otherwise point to the originals!!"

super veryDeepFixupWith: deepCopier.
model := deepCopier references at: model ifAbsent: [model].

]

{ #category : #copying }
PluggableButtonMorph >> veryDeepInner: deepCopier [
	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  	Warning!!  Every instance variable defined in this class must be handled.  We must also implement veryDeepFixupWith:.  See DeepCopier class comment."

super veryDeepInner: deepCopier.
"model := model.		Weakly copied"
label := label veryDeepCopyWith: deepCopier.
icon :=  icon veryDeepCopyWith: deepCopier.
"getStateSelector := getStateSelector.		a Symbol"
"actionSelector := actionSelector.		a Symbol"
"getLabelSelector := getLabelSelector.		a Symbol"
"getMenuSelector := getMenuSelector.		a Symbol"
shortcutCharacter := shortcutCharacter veryDeepCopyWith: deepCopier.
askBeforeChanging := askBeforeChanging veryDeepCopyWith: deepCopier.
triggerOnMouseDown := triggerOnMouseDown veryDeepCopyWith: deepCopier.
offColor := offColor veryDeepCopyWith: deepCopier.
onColor := onColor veryDeepCopyWith: deepCopier.
feedbackColor := feedbackColor veryDeepCopyWith: deepCopier.
showSelectionFeedback := showSelectionFeedback veryDeepCopyWith: deepCopier.
allButtons := nil.		"a cache"
arguments := arguments veryDeepCopyWith: deepCopier.
argumentsProvider := argumentsProvider veryDeepCopyWith: deepCopier.
argumentsSelector := argumentsSelector.  " a Symbol" 
]

{ #category : #keyboard }
PluggableButtonMorph >> wantsKeyboardFocusNavigation [
	"Answer whether the receiver would like keyboard focus
	when navigated to by keyboard."

	^super wantsKeyboardFocusNavigation and: [
		self valueOfProperty: #wantsKeyboardFocusNavigation ifAbsent: [true]]
]

{ #category : #keyboard }
PluggableButtonMorph >> wantsKeyboardFocusOnMouseDown [
	"Answer whether the receiver would like keyboard focus
	on a mouse down event. use a property here for apps that want to take keyboard
	focus when the button is pressed (so that other morphs can, e.g. accept on focus change)."

	^self wantsKeyboardFocus and: [self valueOfProperty: #wantsKeyboardFocusOnMouseDown ifAbsent: [false]]
]
