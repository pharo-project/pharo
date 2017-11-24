"
Checkbox/radio - button only.
"
Class {
	#name : #CheckboxButtonMorph,
	#superclass : #ThreePhaseButtonMorph,
	#traits : 'TEnableOnHaloMenu',
	#classTraits : 'TEnableOnHaloMenu classTrait',
	#instVars : [
		'repressedImage',
		'enabled',
		'isRadioButton',
		'images'
	],
	#category : #'Morphic-Widgets-Basic-Buttons'
}

{ #category : #'instance creation' }
CheckboxButtonMorph class >> checkBox [
	"Answer a button pre-initialized with checkbox images."
	
	^self new
		beCheckbox
]

{ #category : #'instance creation' }
CheckboxButtonMorph class >> radioButton [
	"Answer a button pre-initialized with radio button images."
	
	^self new
		beRadioButton
]

{ #category : #accessing }
CheckboxButtonMorph >> adoptPaneColor: paneColor [
	"Pass on to the border too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self fillStyle: self fillStyleToUse.
	self borderStyle: self borderStyleToUse.
	self cornerStyle: (self isRadioButton
		ifTrue: [self theme radioButtonCornerStyleFor: self]
		ifFalse: [self theme checkboxCornerStyleFor: self])
]

{ #category : #protocol }
CheckboxButtonMorph >> beCheckbox [
	"Change the images and square the border
	to be a checkbox."

	self
		isRadioButton: false;
		onImage: self theme checkboxMarkerForm;
		cornerStyle: (self theme checkboxCornerStyleFor: self);
		fillStyle: self fillStyleToUse;
		borderStyle: self borderStyleToUse
]

{ #category : #protocol }
CheckboxButtonMorph >> beRadioButton [
	"Change the images and round the border
	to be a radio button."

	self
		isRadioButton: true;
		onImage: self theme radioButtonMarkerForm;
		cornerStyle: (self theme radioButtonCornerStyleFor: self);
		fillStyle: self fillStyleToUse;
		borderStyle: self borderStyleToUse
]

{ #category : #accessing }
CheckboxButtonMorph >> borderStyle: newStyle [
	"Use narrowest image dimension."
	
	| newExtent |
	self borderStyle = newStyle ifTrue: [^self].
	super borderStyle: newStyle.
	newExtent := 2 * newStyle width + image extent min asPoint.
	bounds extent = newExtent ifFalse: [self extent: newExtent]
]

{ #category : #accessing }
CheckboxButtonMorph >> borderStyleToUse [
	"Answer the borderStyle that should be used for the receiver."
	
	^self isRadioButton
		ifTrue: [self radioBorderStyleToUse]
		ifFalse: [self checkboxBorderStyleToUse]
]

{ #category : #accessing }
CheckboxButtonMorph >> borderWidth: bw [
	"Use narrowest image dimension."
	
	| newExtent |
	super borderWidth: bw.
	newExtent := 2 * bw + image extent min asPoint.
	bounds extent = newExtent ifFalse: [super extent: newExtent]
]

{ #category : #private }
CheckboxButtonMorph >> checkboxBorderStyleToUse [
	"Answer the borderStyle that should be used for the receiver when it is a checkbox."
	
	^self selected
		ifTrue: [self enabled
			ifTrue: [self theme checkboxButtonSelectedBorderStyleFor: self]
			ifFalse: [self theme checkboxButtonSelectedDisabledBorderStyleFor: self]]
		ifFalse: [self enabled
			ifTrue: [self theme checkboxButtonNormalBorderStyleFor: self]
			ifFalse: [self theme checkboxButtonDisabledBorderStyleFor: self]]
]

{ #category : #private }
CheckboxButtonMorph >> checkboxFillStyleToUse [
	"Answer the fillStyle that should be used for the receiver when it is a checkbox."
	
	^self selected
		ifTrue: [self enabled
			ifTrue: [self theme checkboxButtonSelectedFillStyleFor: self]
			ifFalse: [self theme checkboxButtonSelectedDisabledFillStyleFor: self]]
		ifFalse: [self enabled
			ifTrue: [self theme checkboxButtonNormalFillStyleFor: self]
			ifFalse: [self theme checkboxButtonDisabledFillStyleFor: self]]
]

{ #category : #accessing }
CheckboxButtonMorph >> colorToUse [
	"Answer the color we should use."

	^self paneColor
]

{ #category : #protocol }
CheckboxButtonMorph >> disable [
	"Disable the receiver."
	
	self enabled: false
]

{ #category : #drawing }
CheckboxButtonMorph >> drawOn: aCanvas [
	"Draw the image for the current state."
	
	|img|
	aCanvas fillRectangle: self bounds fillStyle: self fillStyle borderStyle: self borderStyle.
	img := self imageToUse.
	img ifNotNil: [
		aCanvas
			translucentImage: img
			at: self innerBounds center - (img extent // 2)].
	((self state == #pressed or: [self state == #repressed]) and: [image isNil]) ifTrue: [
		aCanvas fillRectangle: self innerBounds fillStyle: (self paneColor alpha: 0.3)].
	(self enabled not and: [self theme fadeCheckboxWhenDisabled]) ifTrue: [
		aCanvas fillRectangle: self innerBounds fillStyle: (self paneColor alpha: 0.4)]
]

{ #category : #protocol }
CheckboxButtonMorph >> enable [
	"Enable the receiver."
	
	self enabled: true
]

{ #category : #accessing }
CheckboxButtonMorph >> enabled [
	"Answer the value of enabled"

	^ enabled
]

{ #category : #accessing }
CheckboxButtonMorph >> enabled: anObject [
	"Set the value of enabled"

	enabled = anObject ifTrue: [^self].
	enabled := anObject.
	self changed: #enabled.
	self
		adoptPaneColor: self paneColor;
		changed
]

{ #category : #accessing }
CheckboxButtonMorph >> fillStyleToUse [
	"Answer the fillStyle that should be used for the receiver."
	
	^self isRadioButton
		ifTrue: [self radioFillStyleToUse]
		ifFalse: [self checkboxFillStyleToUse]
]

{ #category : #accessing }
CheckboxButtonMorph >> image: anImage [ 
	"Fixed to take account of border width. Use narrowest
	dimanesion of image to allow a little flexibility."
	
	image := anImage depth = 1 
				ifTrue: [ColorForm mappingWhiteToTransparentFrom: anImage]
				ifFalse: [anImage]. 
	self extent: 2 * self borderWidth + image extent min asPoint.
	self changed
]

{ #category : #private }
CheckboxButtonMorph >> imageFromName: aSymbol [
	^ self images
		at: aSymbol
		ifPresent: [:block | block value]
		ifAbsent: []
]

{ #category : #private }
CheckboxButtonMorph >> imageToUse [
	"Answer the image we should use."

	^ self imageFromName: state
]

{ #category : #accessing }
CheckboxButtonMorph >> images [
	^ images ifNil: [images := Dictionary newFromPairs: {
								#off . [self offImage] .
								#pressed . [self pressedImage] . 
								#on . [self onImage] .
								#repressed . [self repressedImage ifNil: [self onImage]] }]
]

{ #category : #initialization }
CheckboxButtonMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		isRadioButton: false;
		enabled: true;
		onImage: self theme checkboxMarkerForm;
		fillStyle: self fillStyleToUse;
		borderStyle: self borderStyleToUse
]

{ #category : #accessing }
CheckboxButtonMorph >> isRadioButton [
	"Answer the value of isRadioButton"

	^ isRadioButton
]

{ #category : #accessing }
CheckboxButtonMorph >> isRadioButton: anObject [
	"Set the value of isRadioButton"

	isRadioButton := anObject
]

{ #category : #'event handling' }
CheckboxButtonMorph >> mouseDown: evt [
	"Handle the transitions."
	
	self perform: #mouseDown: withArguments: {evt} inSuperclass: Morph.
	self enabled ifFalse: [^self].
	self isOn
		ifTrue: [self state: #repressed]
		ifFalse: [self state: #pressed].
	actWhen == #buttonDown
		ifTrue:
			[self doButtonAction].
	self mouseStillDown: evt.
]

{ #category : #'event handling' }
CheckboxButtonMorph >> mouseMove: evt [
	"Check for straying."
	
	self perform: #mouseMove: withArguments: {evt} inSuperclass: Morph.
	self enabled ifFalse: [^self].
	(self containsPoint: evt cursorPoint)
		ifTrue: [state == #on
					ifTrue: [self state: #repressed].
				state == #off
					ifTrue: [self state: #pressed]]
		ifFalse: [state == #repressed
					ifTrue: [self state: #on].
				state == #pressed
					ifTrue: [self state: #off]]
]

{ #category : #'event handling' }
CheckboxButtonMorph >> mouseUp: evt [
	"Allow on:send:to: to set the response to events other than actWhen"
	
	self enabled ifFalse: [^self perform: #mouseUp: withArguments: {evt} inSuperclass: Morph].
	actWhen == #buttonUp
		ifFalse: [^self perform: #mouseUp: withArguments: {evt} inSuperclass: Morph].
	(self containsPoint: evt cursorPoint)
		ifTrue: [state == #repressed
					ifTrue: [self state: #off]
					ifFalse: [self state: #on].
				self doButtonAction: evt].
	^self perform: #mouseUp: withArguments: {evt} inSuperclass: Morph
]

{ #category : #private }
CheckboxButtonMorph >> radioBorderStyleToUse [
	"Answer the borderStyle that should be used for the receiver when it is a radio button."
	
	^self selected
		ifTrue: [self enabled
			ifTrue: [self theme radioButtonSelectedBorderStyleFor: self]
			ifFalse: [self theme radioButtonSelectedDisabledBorderStyleFor: self]]
		ifFalse: [self enabled
			ifTrue: [self theme radioButtonNormalBorderStyleFor: self]
			ifFalse: [self theme radioButtonDisabledBorderStyleFor: self]]
]

{ #category : #private }
CheckboxButtonMorph >> radioFillStyleToUse [
	"Answer the fillStyle that should be used for the receiver when it is a radio button."
	
	^self selected
		ifTrue: [self enabled
			ifTrue: [self theme radioButtonSelectedFillStyleFor: self]
			ifFalse: [self theme radioButtonSelectedDisabledFillStyleFor: self]]
		ifFalse: [self enabled
			ifTrue: [self theme radioButtonNormalFillStyleFor: self]
			ifFalse: [self theme radioButtonDisabledFillStyleFor: self]]
]

{ #category : #accessing }
CheckboxButtonMorph >> repressedImage [
	"Answer the value of repressedImage"

	^ repressedImage
]

{ #category : #accessing }
CheckboxButtonMorph >> repressedImage: anObject [
	"Set the value of repressedImage. This is shown when
	pressed after being off."

	repressedImage := anObject.
	self invalidRect: self bounds
]

{ #category : #accessing }
CheckboxButtonMorph >> selected [
	"Answer the state taking account of the intermediate states."

	^self state == #repressed or: [self state == #on]
]

{ #category : #accessing }
CheckboxButtonMorph >> selected: aBoolean [
	"Set the state taking account of the intermediate states."

	(self state == #pressed or: [self state == #repressed])
		ifTrue: [self state: (aBoolean ifTrue: [#repressed] ifFalse: [#pressed])]
		ifFalse: [self state: (aBoolean ifTrue: [#on] ifFalse: [#off])]
]

{ #category : #accessing }
CheckboxButtonMorph >> state [
	"Answer the state."

	^state
]

{ #category : #accessing }
CheckboxButtonMorph >> state: newState [
	"Change the image and invalidate the rect."

	newState == state ifTrue: [^ self].
	state := newState.
	self
		adoptPaneColor: self paneColor;
		changed
]

{ #category : #theme }
CheckboxButtonMorph >> themeChanged [
	"Update the on image."

	self onImage: (self isRadioButton
		ifTrue: [self theme radioButtonMarkerForm]
		ifFalse: [self theme checkboxMarkerForm]).
	self adoptPaneColor: self paneColor.
	super themeChanged
]
