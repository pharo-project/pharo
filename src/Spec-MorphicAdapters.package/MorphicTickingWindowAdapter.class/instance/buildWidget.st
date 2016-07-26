factory
buildWidget
	^ TickingSpecWindow new
			model: self;
			isResizeable: self isResizeable;
			yourself