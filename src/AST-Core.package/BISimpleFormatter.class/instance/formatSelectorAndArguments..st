private-formatting
formatSelectorAndArguments: aMessageNode
	
	self
		indent: 2
		around: [ 
			self
				formatSelectorAndArguments: aMessageNode
				firstSeparator: [ self space ]
				restSeparator: [ self space ] ]