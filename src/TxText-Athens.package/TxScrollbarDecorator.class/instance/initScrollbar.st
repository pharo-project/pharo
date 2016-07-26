initialize
initScrollbar
	| ann |
	self removeAllMorphs.
	scrollbar ifNotNil: [  scrollbar announcer unsubscribe: self ].

	scrollbar := (vertical ifTrue: [ TxSlider vertical ] ifFalse: [ TxSlider horizontal  ]) new.
	
	self addMorph: scrollbar.

	ann := scrollbar announcer weak.
	
	ann 
		when: TxSliderDragged send: #sliderDragged: to: self;
		when: TxSliderScrollBackward send: #sliderScrollBackward to: self;
		when: TxSliderScrollForward send: #sliderScrollForward to: self
	