testing
isAnimated

	borderDashSpec ifNil: [^false].
	^ borderDashSpec size = 5 and: [(borderDashSpec fifth) > 0]