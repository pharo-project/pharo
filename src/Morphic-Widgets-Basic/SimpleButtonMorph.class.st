"
I am labeled, rectangular morph which allows the user to click me. I can be configured to send my ""target"" the message ""actionSelector"" with ""arguments"" when I am clicked. I may have a label, implemented as a StringMorph.

Example:

	SimpleButtonMorph new
		target: Beeper;
		label: 'Beep!';
		actionSelector: #beep; 
		openInWorld

Structure:
instance var 	Type		Description 
target 			Object 		The Object to notify upon a click 
actionSelector 	Symbol 		The message to send to Target (#messageName) 
arguments 		Array 		Arguments to send with #actionSelection (optional) 
actWhen 		Symbol 		When to take action: may be #buttonUp (default), #buttonDown,
								#whilePressed, or #startDrag 
oldColor 		Color 		Used to restore color after click 

Another example: a button which quits the image without saving it.

	SimpleButtonMorph new
		target: Smalltalk;
		label: 'quit';
		actionSelector: #snapshot:andQuit:;
		arguments: (Array with: false with: true); 
		openInWorld


"
Class {
	#name : #SimpleButtonMorph,
	#superclass : #BorderedMorph,
	#traits : 'TAbleToRotate',
	#classTraits : 'TAbleToRotate classTrait',
	#instVars : [
		'target',
		'actionSelector',
		'arguments',
		'actWhen',
		'oldColor',
		'label'
	],
	#category : #'Morphic-Widgets-Basic-Buttons'
}

{ #category : #'instance creation' }
SimpleButtonMorph class >> newWithLabel: labelString [

	^(self new) 
		label: labelString;
		yourself

]

{ #category : #'submorphs-add/remove' }
SimpleButtonMorph >> actWhen [
	"acceptable symbols:  #buttonDown, #buttonUp, and #whilePressed"

	^ actWhen
]

{ #category : #'submorphs-add/remove' }
SimpleButtonMorph >> actWhen: condition [
	"Accepts symbols:  #buttonDown, #buttonUp, and #whilePressed, #startDrag"
	actWhen := condition.
	actWhen == #startDrag
		ifFalse: [self on: #startDrag send: nil to: nil ]
		ifTrue:[self on: #startDrag send: #doButtonAction to: self].
]

{ #category : #accessing }
SimpleButtonMorph >> actionSelector [

	^ actionSelector

]

{ #category : #accessing }
SimpleButtonMorph >> actionSelector: aSymbolOrString [

	(nil = aSymbolOrString or:
	 ['nil' = aSymbolOrString or:
	 [aSymbolOrString isEmpty]])
		ifTrue: [^ actionSelector := nil].

	actionSelector := aSymbolOrString asSymbol.

]

{ #category : #menu }
SimpleButtonMorph >> addCustomMenuItems: aCustomMenu hand: aHandMorph [

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	self addLabelItemsTo: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'change action selector' selector: #setActionSelector.
	aCustomMenu add: 'change arguments' selector: #setArguments.
	aCustomMenu add: 'change when to act' selector: #setActWhen.
	self addTargetingMenuItems: aCustomMenu hand: aHandMorph.

]

{ #category : #menu }
SimpleButtonMorph >> addLabelItemsTo: aCustomMenu hand: aHandMorph [ 
	aCustomMenu add: 'change label' selector: #setLabel
]

{ #category : #menu }
SimpleButtonMorph >> addTargetingMenuItems: aCustomMenu hand: aHandMorph [ 
	"Add targeting menu items"
	aCustomMenu addLine.
	aCustomMenu add: 'set target' selector: #targetWith:.
	target
		ifNotNil: [aCustomMenu add: 'clear target' translated selector: #clearTarget]
]

{ #category : #accessing }
SimpleButtonMorph >> arguments [

	^ arguments

]

{ #category : #accessing }
SimpleButtonMorph >> arguments: aCollection [

	arguments := aCollection asArray copy.

]

{ #category : #menu }
SimpleButtonMorph >> clearTarget [

	target := nil.

]

{ #category : #initialization }
SimpleButtonMorph >> defaultLabel [
	^ 'Flash'.

]

{ #category : #button }
SimpleButtonMorph >> doButtonAction [
	"Perform the action of this button. Subclasses may override this method. The default behavior is to send the button's actionSelector to its target object with its arguments."

	(target notNil and: [actionSelector notNil]) 
		ifTrue: 
			[Cursor normal 
				showWhile: [target perform: actionSelector withArguments: arguments]].
	actWhen == #startDrag ifTrue: [oldColor ifNotNil: [self color: oldColor]]
]

{ #category : #geometry }
SimpleButtonMorph >> extent: newExtent [

	super extent: newExtent.
	label ifNotNil: [label position: self center - (label extent // 2)].
]

{ #category : #accessing }
SimpleButtonMorph >> fitContents [
	| aMorph aCenter |
	aCenter := self center.
	submorphs isEmpty ifTrue: [^self].
	aMorph := submorphs first.
	self extent: aMorph extent + (borderWidth + 6).
	self center: aCenter.
	aMorph position: aCenter - (aMorph extent // 2)
]

{ #category : #'event handling' }
SimpleButtonMorph >> handlesMouseDown: evt [
	^  true

]

{ #category : #'event handling' }
SimpleButtonMorph >> handlesMouseStillDown: evt [
	^actWhen == #whilePressed
]

{ #category : #balloon }
SimpleButtonMorph >> helpText [

	^ self balloonText
]

{ #category : #balloon }
SimpleButtonMorph >> helpText: aString [

	self setBalloonText: aString
]

{ #category : #initialization }
SimpleButtonMorph >> initialize [
	super initialize.

	self borderWidth: 1.
	self color: (Color r: 0.4 g: 0.8 b: 0.6).
	self borderColor: self color darker.
	self borderStyle: BorderStyle thinGray.
	target := nil.
	actionSelector := #flash.
	arguments := EmptyArray.
	actWhen := #buttonUp.
	self setDefaultLabel 
]

{ #category : #testing }
SimpleButtonMorph >> isDefault [
	^ self extension isDefault
]

{ #category : #accessing }
SimpleButtonMorph >> label [
	^ label contents
]

{ #category : #accessing }
SimpleButtonMorph >> label: aString [
	self label: aString font: StandardFonts buttonFont
]

{ #category : #accessing }
SimpleButtonMorph >> label: aString font: aFont [

	label ifNotNil: [label delete].
	label := StringMorph contents: aString font: (aFont ifNil: [StandardFonts buttonFont]).
	self extent: (label width + 6) @ (label height + 6).
	label position: self center - (label extent // 2).
	self addMorph: label.
	label lock

]

{ #category : #accessing }
SimpleButtonMorph >> labelString: aString [

	label ifNil: [self label: aString]
		ifNotNil:
			[label contents: aString.
			self fitContents]

]

{ #category : #'event handling' }
SimpleButtonMorph >> mouseDown: evt [

	super mouseDown: evt.
	evt yellowButtonPressed ifTrue: [ ^self ] .
	oldColor := self fillStyle. 
	actWhen == #buttonDown
		ifTrue: [ self doButtonAction]
		ifFalse: [ self updateVisualState: evt ].
	self mouseStillDown: evt.
]

{ #category : #'event handling' }
SimpleButtonMorph >> mouseMove: evt [
	actWhen == #buttonDown ifTrue: [^ self].
	self updateVisualState: evt.
]

{ #category : #'event handling' }
SimpleButtonMorph >> mouseStillDown: evt [
	actWhen == #whilePressed ifFalse:[^self].
	(self containsPoint: evt cursorPoint) ifTrue:[self doButtonAction].
]

{ #category : #'events-processing' }
SimpleButtonMorph >> mouseStillDownStepRate [
	"Answer how often I want the #handleMouseStillDown: stepped"
	^200
]

{ #category : #'event handling' }
SimpleButtonMorph >> mouseUp: evt [
	super mouseUp: evt.
	oldColor ifNotNil:
		["if oldColor nil, it signals that mouse had not gone DOWN
		inside me, e.g. because of a cmd-drag; in this case we want
		to avoid triggering the action!"
		self color: oldColor.
		oldColor := nil.
		(self containsPoint: evt cursorPoint) 
				ifTrue: [ actWhen == #buttonUp 
							ifTrue: [self doButtonAction]  ]
				ifFalse: [ self mouseLeave: evt "This is a balk. Note that we have left." ]]
		

]

{ #category : #menu }
SimpleButtonMorph >> setActWhen [
	| selections |
	selections := #(#buttonDown #buttonUp #whilePressed #startDrag ).
	actWhen := UIManager default
				chooseFrom: (selections 
						collect: [:t | t translated])
				values: selections
				title: 'Choose one of the following conditions' translated
]

{ #category : #menu }
SimpleButtonMorph >> setActionSelector [

	| newSel |
	newSel := UIManager default
		request:
'Please type the selector to be sent to
the target when this button is pressed' translated
		initialAnswer: actionSelector.
	newSel isEmptyOrNil ifFalse: [self actionSelector: newSel].

]

{ #category : #menu }
SimpleButtonMorph >> setArguments [
	| newArgs newArgsArray |
	newArgs := UIManager default
		request:
			'Please type the arguments to be sent to the target
when this button is pressed separated by periods' translated
		initialAnswer: (String streamContents: [ :str | arguments do: [ :arg | arg printOn: str. str nextPutAll: '. ' ] ]).
	newArgs isEmptyOrNil ifFalse: [ 
			newArgsArray := self class compiler
				source: '{' , newArgs , '}';
				receiver: self;
				evaluate.
			self arguments: newArgsArray ]
]

{ #category : #initialization }
SimpleButtonMorph >> setDefaultLabel [
	self label: self defaultLabel

]

{ #category : #menu }
SimpleButtonMorph >> setLabel [

	| newLabel |
	newLabel := UIManager default
		request: 'Please enter a new label for this button' translated
		initialAnswer: self label.
	newLabel isEmptyOrNil ifFalse: [self labelString: newLabel].

]

{ #category : #menu }
SimpleButtonMorph >> setTarget: evt [ 
	| rootMorphs |
	rootMorphs := self world rootMorphsAt: evt hand targetPoint.
	target := rootMorphs size > 1
				ifTrue: [rootMorphs second]
]

{ #category : #accessing }
SimpleButtonMorph >> target [

	^ target

]

{ #category : #accessing }
SimpleButtonMorph >> target: anObject [

	target := anObject

]

{ #category : #theme }
SimpleButtonMorph >> themeChanged [

	self
	layoutInset: (self theme buttonLabelInsetFor: self);
	cornerStyle: (self theme buttonCornerStyleIn: self window);
	fillStyle: self themedFillStyle.
	
	label ifNotNil: [ label color: self fillStyle asColor contrastingColor ].

	super themeChanged
]

{ #category : #theme }
SimpleButtonMorph >> themedFillStyle [

	^self theme buttonNormalFillStyleFor: self
]

{ #category : #'visual properties' }
SimpleButtonMorph >> updateVisualState: evt [
	
	oldColor ifNotNil: [
		 self color: 
			((self containsPoint: evt cursorPoint)
				ifTrue: [oldColor mixed: 0.5 with: Color white]
				ifFalse: [oldColor])]

]

{ #category : #copying }
SimpleButtonMorph >> veryDeepFixupWith: deepCopier [
	"If target and arguments fields were weakly copied, fix them here.  If they were in the tree being copied, fix them up, otherwise point to the originals!!"

super veryDeepFixupWith: deepCopier.
target := deepCopier references at: target ifAbsent: [target].
arguments := arguments collect: [:each |
	deepCopier references at: each ifAbsent: [each]].

]

{ #category : #copying }
SimpleButtonMorph >> veryDeepInner: deepCopier [
	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  	Warning!!  Every instance variable defined in this class must be handled.  We must also implement veryDeepFixupWith:.  See DeepCopier class comment."

super veryDeepInner: deepCopier.
"target := target.		Weakly copied"
"actionSelector := actionSelector.		a Symbol"
"arguments := arguments.		All weakly copied"
actWhen := actWhen veryDeepCopyWith: deepCopier.
oldColor := oldColor veryDeepCopyWith: deepCopier.
label := label veryDeepCopyWith: deepCopier.
]
