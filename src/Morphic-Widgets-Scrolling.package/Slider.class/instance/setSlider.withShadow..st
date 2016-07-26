initialization
setSlider: aMorph withShadow: anotherMorph
	

	slider ifNotNil: [ self removeMorph: slider ].
	sliderShadow ifNotNil: [ self pagingArea removeMorph: sliderShadow ].

	slider := aMorph.
	sliderShadow := anotherMorph.
	slider on: #mouseMove send: #scrollAbsolute: to: self.
	slider on: #mouseDown send: #mouseDownInSlider: to: self.
	slider on: #mouseUp send: #mouseUpInSlider: to: self.

	"(the shadow must have the pagingArea as its owner to highlight properly)"
	self pagingArea addMorph: sliderShadow.
	sliderShadow hide.
	self addMorph: slider.
	self computeSlider.