"
I represent the colored feedback to alert a too long method
"
Class {
	#name : #RubCodeSizeFeedback,
	#superclass : #RubScrolledTextExtraSurface,
	#category : #'Rubric-Editing-Widgets'
}

{ #category : #querying }
RubCodeSizeFeedback class >> key [
	^ #codeSizeFeedback
]

{ #category : #accessing }
RubCodeSizeFeedback >> alertLimit [

	^ 2 * self warningLimit
]

{ #category : #accessing }
RubCodeSizeFeedback >> backgroundColor [
	"Return the current fillStyle of the receiver."

	| basicColor size |
	basicColor := self theme backgroundColor.
	self paragraph ifNil: [ ^Color transparent ].
	size := self textSize.
	size >= self warningLimit
		ifTrue: [ 
			|  overshoot transitionSpan transitionColor |
			transitionSpan := self warningLimit.
			transitionColor := Color yellow mixed: 0.6 with: basicColor.
			size >= self alertLimit
				ifFalse: [ overshoot := size - self warningLimit ]
				ifTrue: [ 
					transitionSpan := self alertLimit - self warningLimit.
					basicColor := transitionColor mixed: 0.5 with: basicColor.
					transitionColor := (Color orange mixed: 0.5 with: Color red) mixed: 0.5 with: basicColor.
					overshoot := size - self alertLimit ].
			^ CompositeFillStyle
				fillStyles:
					{((GradientFillStyle
						ramp:
							{(0 -> transitionColor).
							(1 -> basicColor)})
						origin: (self bounds width / 2) asFloat @ self bounds bottom;
						direction: 0 @ (12 + (2 * self bounds height * overshoot / transitionSpan)) negated asFloat;
						normal: 0 @ 1;
						radial: false;
						yourself).
					((GradientFillStyle
						ramp:
							{(0 -> self theme backgroundColor).
							(0.3 -> (self theme backgroundColor alpha: 0.01))})
						origin: self bounds left @ (self bounds height / 2) asFloat;
						direction: self bounds width @ 0;
						normal: 1 @ 0;
						radial: false;
						yourself)} ].
	^ self normalColor
]

{ #category : #accessing }
RubCodeSizeFeedback >> computedPositionFrom: aRectangle [
	^  ((aRectangle right min: self scrollBounds right) - (self computedWidthFrom: aRectangle)) @ aRectangle top

]

{ #category : #accessing }
RubCodeSizeFeedback >> computedWidthFrom: aRectangle [
	^  (super computedWidthFrom: aRectangle) min: 50.

]

{ #category : #defaults }
RubCodeSizeFeedback >> defaultBackgroundColor [
	^ self backgroundColor 
]

{ #category : #drawing }
RubCodeSizeFeedback >> drawOn: aCanvas [
	| font s |
	super drawOn: aCanvas.
	color = self normalColor
		ifTrue: [ ^ self ].
	font := self paragraph textStyle defaultFont.
	s := self textSize asString.
	aCanvas
		drawString: s
		in: (self bounds withTop: self bounds bottom - font height)
		font: font
		color: self stringColor
]

{ #category : #drawing }
RubCodeSizeFeedback >> drawOnAthensCanvas: anAthensCanvas [
	| font s bnds |
	super drawOnAthensCanvas: anAthensCanvas.
	color = self normalColor
		ifTrue: [ ^ self ].
	font := self paragraph textStyle defaultFont.
	s := self textSize asString.
	bnds := self bounds withTop: self bounds bottom - font height.
	anAthensCanvas morphicDrawString: s in: bnds font: font color: self stringColor
]

{ #category : #geometry }
RubCodeSizeFeedback >> extent: aPoint [ 
	self updateColor.
	super extent: aPoint
]

{ #category : #private }
RubCodeSizeFeedback >> ignoredCharacters [

	^ { Character space. Character tab. Character cr. $.. $;. $:. $'. $(. $) }
]

{ #category : #accessing }
RubCodeSizeFeedback >> normalColor [
	^ Color transparent
]

{ #category : #'event handling' }
RubCodeSizeFeedback >> scrollerOffsetChanged [
	self updateColor.
	super scrollerOffsetChanged
]

{ #category : #accessing }
RubCodeSizeFeedback >> stringColor [
	| size |
	size := self textSize.
	self warningLimit negative ifTrue: [ ^ Color black ].
	^ size >= self warningLimit
			ifTrue: [
				size >= self alertLimit
					ifTrue: [ Color red ]
					ifFalse: [ Color orange ]]
			ifFalse: [ Color black ].
]

{ #category : #'event handling' }
RubCodeSizeFeedback >> textChanged [
	self updateColor.
	super textChanged
]

{ #category : #private }
RubCodeSizeFeedback >> textSize [
	| result count ignoredCharacters |
	result := 0.
	count := true.
	ignoredCharacters := self ignoredCharacters.
	self text string
		do: [ :char | 
			result > (self alertLimit * 3) 
				ifTrue: [ ^result ].
			(#($' $") includes: char)
				ifTrue: [ count := count not ]
				ifFalse: [ 
					((ignoredCharacters includes: char) not and: [ count ])
						ifTrue: [ result := result + 1 ] ] ].
	^ result
]

{ #category : #private }
RubCodeSizeFeedback >> updateColor [
	self color: self backgroundColor
]

{ #category : #accessing }
RubCodeSizeFeedback >> warningLimit [

	^ 350
]
