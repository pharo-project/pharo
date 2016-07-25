*Morphic-Base
exampleGradient
	"self exampleGradient"
	
	| morph fs |
	morph := BorderedMorph new.
	fs := GradientFillStyle ramp: {0.0 -> Color red. 1.0 -> Color green}.
	fs origin: morph bounds center.
	fs direction: (morph bounds width // 2) @ 0.
	fs radial: true.
	morph fillStyle: fs.
	World primaryHand attachMorph: morph.