layout
acceptDroppingMorph: aMorph event: evt
	"This message is sent when a morph is dropped onto me."
	self addMorphFront: aMorph fromWorldPosition: aMorph position.
		"Make a TextAnchor and install it in a run."