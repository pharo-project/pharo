"
A button morph with separate images for on, off, and pressed with the mouse. 

When the event actWhen occurs, send actionSelector with 'arguments' to target.  For other events, default to my eventHandler.  The current event is not supplied in the arguments to the actionSelector.  

image (a.k.a. onImage) may not be nil.  offImage and pressedImage may be nil.  nil there means be transparent and show the underlying object.  

Tools for debugging:
Display the images momentarily under program control (for positioning) (self is an instance).
	self state: #on.  self state: #off.
	self state: #pressed.  self state: #off.
Display a rectangle where the button is.
	Display fillWithColor: bounds + (self world viewBox origin).
	self invalidRect: bounds.
"
Class {
	#name : #ThreePhaseButtonMorph,
	#superclass : #ImageMorph,
	#instVars : [
		'offImage',
		'pressedImage',
		'state',
		'target',
		'actionSelector',
		'arguments',
		'actWhen'
	],
	#category : #'Morphic-Widgets-Basic-Buttons'
}

{ #category : #'instance creation' }
ThreePhaseButtonMorph class >> checkBox [
	"Answer a button pre-initialized with checkbox images."

	| f |
	^ self new
		onImage: (f := self iconNamed: #checkBoxOnIcon);
		pressedImage: (self iconNamed: #checkBoxPressedIcon);
		offImage: (self iconNamed: #checkBoxOffIcon);
		extent: f extent + (2 @ 0);
		yourself
]

{ #category : #'instance creation' }
ThreePhaseButtonMorph class >> radioButton [
	"Answer a button pre-initialized with radiobutton images."

	| f |
	^ self new
		onImage: (f := self iconNamed: #radioButtonOnIcon);
		pressedImage: (self iconNamed: #radioButtonPressedIcon);
		offImage: (self iconNamed: #radioButtonOffIcon);
		extent: f extent + (2 @ 0);
		yourself
]

{ #category : #'submorphs-add/remove' }
ThreePhaseButtonMorph >> actWhen: condition [
	"Accepts symbols:  #buttonDown, #buttonUp, and #whilePressed"
	actWhen := condition
]

{ #category : #accessing }
ThreePhaseButtonMorph >> actionSelector [

	^ actionSelector

]

{ #category : #accessing }
ThreePhaseButtonMorph >> actionSelector: aSymbolOrString [

	(nil = aSymbolOrString or:
	 ['nil' = aSymbolOrString or:
	 [aSymbolOrString isEmpty]])
		ifTrue: [^ actionSelector := nil].

	actionSelector := aSymbolOrString asSymbol.

]

{ #category : #accessing }
ThreePhaseButtonMorph >> arguments [
	^ arguments
]

{ #category : #accessing }
ThreePhaseButtonMorph >> arguments: aCollection [

	arguments := aCollection asArray copy.

]

{ #category : #button }
ThreePhaseButtonMorph >> doButtonAction [
	"Perform the action of this button. Subclasses may override this method. The default behavior is to send the button's actionSelector to its target object with its arguments."

	(target notNil and: [actionSelector notNil]) 
		ifTrue: 
			[Cursor normal 
				showWhile: [target perform: actionSelector withArguments: arguments].
			target isMorph ifTrue: [target changed]]
]

{ #category : #'event handling' }
ThreePhaseButtonMorph >> doButtonAction: evt [
	
	"Perform the action of this button. Subclasses may override this method. The default behavior is to send the button's actionSelector to its target object with its arguments."

	target ifNil: [^self].
	actionSelector ifNil: [^self].
	Cursor normal showWhile: [ | moreArgs |
		moreArgs := actionSelector numArgs > arguments size ifTrue: [
			arguments copyWith: evt
		] ifFalse: [
			arguments
		].
		target perform: actionSelector withArguments: moreArgs
	]
]

{ #category : #drawing }
ThreePhaseButtonMorph >> drawOn: aCanvas [

	state == #off ifTrue: [
		offImage ifNotNil: [aCanvas translucentImage: offImage at: bounds origin]].
	state == #pressed ifTrue: [
		pressedImage ifNotNil: [aCanvas translucentImage: pressedImage at: bounds origin]].
	state == #on ifTrue: [
		image ifNotNil: [aCanvas translucentImage: image at: bounds origin]].
]

{ #category : #geometry }
ThreePhaseButtonMorph >> extent: aPoint [
	"Do it normally"
	
	self changed.
	bounds := bounds topLeft extent: aPoint.
	self layoutChanged.
	self changed.

]

{ #category : #'event handling' }
ThreePhaseButtonMorph >> handlesMouseDown: evt [

	^ true

]

{ #category : #'event handling' }
ThreePhaseButtonMorph >> handlesMouseStillDown: evt [
	^actWhen == #whilePressed
]

{ #category : #balloon }
ThreePhaseButtonMorph >> helpText [
	
	^self balloonText
]

{ #category : #balloon }
ThreePhaseButtonMorph >> helpText: aString [

	self setBalloonText: aString
]

{ #category : #initialization }
ThreePhaseButtonMorph >> initialize [
	super initialize.
	state := #off.
	actionSelector := #flash.
	arguments := EmptyArray.
	actWhen := #buttonUp

	"self on: #mouseStillDown send: #dragIfAuthoring: to: self."
	"real move should include a call on dragIfAuthoring: "
]

{ #category : #testing }
ThreePhaseButtonMorph >> isOn [
	^ state == #on
]

{ #category : #'event handling' }
ThreePhaseButtonMorph >> mouseDown: evt [
	| now dt |
	self state: #pressed.
	actWhen == #buttonDown
		ifTrue:
			[self doButtonAction]
		ifFalse:
			[now := Time millisecondClockValue.
			super mouseDown: evt.
			"Allow on:send:to: to set the response to events other than actWhen"
			dt := Time millisecondClockValue - now max: 0.  "Time it took to do"
			dt < 200 ifTrue: [(Delay forMilliseconds: 200-dt) wait]].
	self mouseStillDown: evt.
]

{ #category : #'event handling' }
ThreePhaseButtonMorph >> mouseMove: evt [
	(self containsPoint: evt cursorPoint)
		ifTrue: [self state: #pressed.
				super mouseMove: evt]
				"Allow on:send:to: to set the response to events other than actWhen"
		ifFalse: [self state: #off].

]

{ #category : #'event handling' }
ThreePhaseButtonMorph >> mouseStillDown: evt [
	actWhen == #whilePressed ifFalse:[^self].
	(self containsPoint: evt cursorPoint) ifTrue:[self doButtonAction].
]

{ #category : #'event handling' }
ThreePhaseButtonMorph >> mouseUp: evt [
	"Allow on:send:to: to set the response to events other than actWhen"
	actWhen == #buttonUp ifFalse: [^super mouseUp: evt].

	(self containsPoint: evt cursorPoint) ifTrue: [
		self state: #on.
		self doButtonAction: evt
	] ifFalse: [
		self state: #off.
	].
	"Allow owner to keep it selected for radio buttons"

]

{ #category : #accessing }
ThreePhaseButtonMorph >> offImage [
	^ offImage
]

{ #category : #accessing }
ThreePhaseButtonMorph >> offImage: aForm [
	offImage := aForm.
	self invalidRect: self bounds.
]

{ #category : #accessing }
ThreePhaseButtonMorph >> onImage [
	^ image
]

{ #category : #button }
ThreePhaseButtonMorph >> onImage: aForm [
	"The main image is used when on.
	Go through ImageMorph method to set extent."
	
	self image: aForm
]

{ #category : #accessing }
ThreePhaseButtonMorph >> pressedImage [
	^ pressedImage
]

{ #category : #accessing }
ThreePhaseButtonMorph >> pressedImage: aForm [
	pressedImage := aForm.
	self invalidRect: self bounds.
]

{ #category : #printing }
ThreePhaseButtonMorph >> printOn: aStream [ 
	| string |
	aStream nextPutAll: '3PButton'.
	arguments notEmpty 
		ifTrue: [string := arguments at: (2 min: arguments size)].
	aStream nextPutAll: '('.
	(string notNil and: [string ~~ self]) 
		ifTrue: 
			[aStream
				print: string;
				space]
		ifFalse: 
			[aStream
				print: actionSelector;
				space].
	aStream
		print: self identityHash;
		nextPutAll: ')'
]

{ #category : #accessing }
ThreePhaseButtonMorph >> state: newState [
	"Change the image and invalidate the rect."

	newState == state ifTrue: [^ self].
	state := newState.
	self invalidRect: bounds.	"All three images must be the same size"
]

{ #category : #stepping }
ThreePhaseButtonMorph >> step [
	(self hasProperty: #doesButtonAction) ifTrue:[
		self doButtonAction.
		self setProperty: #didButtonAction toValue: true.
	].
]

{ #category : #stepping }
ThreePhaseButtonMorph >> stepTime [
	(self hasProperty: #doesButtonAction) ifTrue:[^1].
	^super stepTime
]

{ #category : #accessing }
ThreePhaseButtonMorph >> target [

	^ target

]

{ #category : #accessing }
ThreePhaseButtonMorph >> target: anObject [

	target := anObject

]

{ #category : #copying }
ThreePhaseButtonMorph >> veryDeepFixupWith: deepCopier [
	"If target and arguments fields were weakly copied, fix them here.  If they were in the tree being copied, fix them up, otherwise point to the originals!!"

super veryDeepFixupWith: deepCopier.
target := deepCopier references at: target ifAbsent: [target].
arguments := arguments collect: [:each |
	deepCopier references at: each ifAbsent: [each]].

]

{ #category : #copying }
ThreePhaseButtonMorph >> veryDeepInner: deepCopier [
	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  	Warning!!  Every instance variable defined in this class must be handled.  We must also implement veryDeepFixupWith:.  See DeepCopier class comment."

super veryDeepInner: deepCopier.
offImage := offImage veryDeepCopyWith: deepCopier.
pressedImage := pressedImage veryDeepCopyWith: deepCopier.
state := state veryDeepCopyWith: deepCopier.
"target := target.		Weakly copied"
"actionSelector := actionSelector.		Symbol"
"arguments := arguments.		Weakly copied"
actWhen := actWhen.		"Symbol"
]

{ #category : #stepping }
ThreePhaseButtonMorph >> wantsSteps [
	^self hasProperty: #doesButtonAction
]
