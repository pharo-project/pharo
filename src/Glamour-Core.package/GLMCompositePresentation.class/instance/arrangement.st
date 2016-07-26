accessing
arrangement
	^ arrangement ifNil: [arrangement := GLMTabbedArrangement of: self]