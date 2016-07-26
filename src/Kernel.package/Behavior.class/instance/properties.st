accessing-properties
properties
	^ ClassProperties at: self ifAbsent: nil
		