accessing
searchBackwards: aBoolean
	self searchBackwards = aBoolean
		ifFalse: [self state searchBackwards: aBoolean.
			self state updateFindStartIndexForTextMorph: self model]