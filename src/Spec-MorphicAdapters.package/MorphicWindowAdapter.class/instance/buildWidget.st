factory
buildWidget
	^ SpecWindow new
		model: model;
		isResizeable: self isResizeable;
		yourself