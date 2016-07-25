accessing
withErrorsDo: aBlock
	self errorPropertySelectors
		do: [ :selector | 
			self errorWhileAccessing: selector do: aBlock ].