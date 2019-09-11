Extension { #name : #GradientFillStyle }

{ #category : #'*Morphic-Base-Balloon' }
GradientFillStyle >> addFillStyleMenuItems: aMenu hand: aHand from: aMorph [
	"Add the items for changing the current fill style of the receiver"
	self isRadialFill 
		ifTrue: [ aMenu add: 'linear gradient' target: self selector: #beLinearGradientIn: argument: aMorph ] 		ifFalse: [ aMenu add: 'radial gradient' target: self selector: #beRadialGradientIn: argument: aMorph ].
	aMenu addLine.
	aMenu add: 'change first color' target: self selector: #changeFirstColorIn:event: argument: aMorph.
	aMenu add: 'change second color' target: self selector: #changeSecondColorIn:event: argument: aMorph.
	aMenu addLine.
	super addFillStyleMenuItems: aMenu hand: aHand from: aMorph.
]

{ #category : #'*Morphic-Base-Balloon' }
GradientFillStyle >> addNewColorIn: aMorph event: evt [
	^self inform:'not yet implemented'
]

{ #category : #'*Morphic-Base-Balloon' }
GradientFillStyle >> beLinearGradientIn: aMorph [
	self radial: false.
	aMorph changed.
]

{ #category : #'*Morphic-Base-Balloon' }
GradientFillStyle >> beRadialGradientIn: aMorph [
	self radial: true.
	aMorph changed.
]

{ #category : #'*Morphic-Base-Balloon' }
GradientFillStyle >> changeColorAt: rampIndex to: aColor [
	| ramp |
	ramp := colorRamp deepCopy.
	(ramp at: rampIndex) value: aColor.
	
	colorRamp := ramp.
	isTranslucent := nil.
	pixelRamp := nil.
]

{ #category : #'*Morphic-Base-Balloon' }
GradientFillStyle >> changeColorOf: aMorph rampIndex: rampIndex [
	| originalColor |
	originalColor := (colorRamp at: rampIndex) value.
	Smalltalk ui theme 
		chooseColorIn: self currentWorld title: 'Color' color: originalColor for: [:color |
		self changeColorAt: rampIndex  to:  color.
		aMorph changed] 
]

{ #category : #'*Morphic-Base-Balloon' }
GradientFillStyle >> changeColorSelector: aSymbol hand: aHand morph: aMorph originalColor: originalColor [
	"Change either the firstColor or the lastColor (depending on aSymbol).  Put up a color picker to hande it.  We always use a modal picker so that the user can adjust both colors concurrently."
	|arraySelector|
	"A bad hack, I know... but it already uses a perform which depends on only firstColor: ... / lastColor: ... being sent"
	arraySelector := (aSymbol readStream upTo: $C) asSymbol.
(	UIManager default chooseColor: (colorRamp perform: arraySelector) value)
		ifNotNil: [:nc | 
			self 
				perform: aSymbol 
				with: nc 
				with: aMorph 
				with: aHand]


]

{ #category : #'*Morphic-Base-Balloon' }
GradientFillStyle >> changeFirstColorIn: aMorph event: evt [
	^self changeColorOf: aMorph rampIndex: 1
]

{ #category : #'*Morphic-Base-Balloon' }
GradientFillStyle >> changeSecondColorIn: aMorph event: evt [
	^self changeColorOf: aMorph rampIndex: 2
]

{ #category : #'*Morphic-Base-Balloon' }
GradientFillStyle >> copyWith: aColor atRamp: rampIndex [
	| ramp |
	ramp := colorRamp deepCopy.
	(ramp at: rampIndex) value: aColor.
	^(self class ramp: ramp) 
			origin: self origin;
			direction: self direction;
			normal: self normal;
			radial: self radial;
			yourself
]
