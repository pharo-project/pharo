private
setPaintColor: aColor
	"Install a new color used for filling."
	
	| paintColor screen patternWord |
	paintColor := aColor.
	paintColor ifNil: [paintColor := Color transparent].
	paintColor isColor ifFalse: [ 
		(paintColor isKindOf: InfiniteForm) 
			ifFalse: [^self error:'Cannot install color'].
		^port fillPattern: paintColor; combinationRule: Form paint].
	"Okay, so paintColor really *is* a color"
	port sourceForm: nil.
	(paintColor isTranslucent) ifFalse: [
		port fillPattern: paintColor.
		port combinationRule: Form paint.
		self depth = 8 ifTrue: [
			port fillColor: (form balancedPatternFor: paintColor)].
		^self].
	"paintColor is translucent color"

	self depth > 8 ifTrue: [
		"BitBlt setup for alpha mapped transfer"
		port fillPattern: paintColor.
		self depth = 16
			ifTrue: [port alphaBits: paintColor privateAlpha; combinationRule: 31]
			ifFalse: [port combinationRule: Form blend].
		^self].

	"Can't represent actual transparency -- use stipple pattern"
	screen := self translucentMaskFor: paintColor alpha depth: self depth.
	patternWord := form pixelWordFor: paintColor.
	port fillPattern: (screen collect: [:maskWord | maskWord bitAnd: patternWord]).
	port combinationRule: Form paint
