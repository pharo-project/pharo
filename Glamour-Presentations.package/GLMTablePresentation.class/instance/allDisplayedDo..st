validation
allDisplayedDo: aBlock
	self displayValue do: [ :eachRoot | 
		self withChildrenOf: eachRoot do: aBlock ]