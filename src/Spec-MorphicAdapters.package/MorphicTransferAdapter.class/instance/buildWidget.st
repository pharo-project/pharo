factory
buildWidget

	^ SpecTransferMorph new
		model: self;
		passenger: self passenger;
		source: self source;
		yourself