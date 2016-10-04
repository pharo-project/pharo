updating screen
present
	self validate ifFalse: [ ^ self ].
	renderer
		clear;
		copy: texture;
		present