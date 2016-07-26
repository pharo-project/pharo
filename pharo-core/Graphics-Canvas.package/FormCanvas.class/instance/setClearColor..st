private
setClearColor: aColor
	"Install a new clear color - e.g., a color is used for clearing the background"
	| clearColor |
	clearColor := aColor ifNil:[Color transparent].
	clearColor isColor ifFalse:[
		(clearColor isKindOf: InfiniteForm) ifFalse:[^self error:'Cannot install color'].
		^port fillPattern: clearColor; combinationRule: Form over].
	"Okay, so clearColor really *is* a color"
	port sourceForm: nil.
	port combinationRule: Form over.
	port fillPattern: clearColor.
	self depth = 8 ifTrue:[
		"Use a stipple pattern"
		port fillColor: (form balancedPatternFor: clearColor)].
