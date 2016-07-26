instance creation
extent: extent depth: depth origin: aPoint clipRect: aRectangle

	^ self new
		setForm: (Form extent: extent depth: depth);
		setOrigin: aPoint clipRect: aRectangle;
		yourself