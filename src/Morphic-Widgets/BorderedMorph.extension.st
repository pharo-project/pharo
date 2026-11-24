Extension { #name : 'BorderedMorph' }

{ #category : '*Morphic-Widgets' }
BorderedMorph >> changeBorderColor: evt [
	self theme
		chooseColorIn: self
		title: 'Choose Color' translated
		color: self borderStyle color
		for: [ :newColor | self borderColor: newColor ]
]

{ #category : '*Morphic-Widgets' }
BorderedMorph class >> exampleGradient [

	| morph fs |
	morph := self new.
	fs := GradientFillStyle ramp: {0.0 -> Color red. 1.0 -> Color green}.
	fs
		origin: morph bounds center;
		direction: (morph bounds width // 2) @ 0;
		radial: true.
	morph
		fillStyle: fs;
		openInWindow
]
