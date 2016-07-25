levels
instanceScope

	^ OCInstanceScope new
		slots: class allSlots;
		outerScope: self;
		yourself