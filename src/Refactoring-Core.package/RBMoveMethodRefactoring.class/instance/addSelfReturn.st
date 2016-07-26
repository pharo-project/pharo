transforming
addSelfReturn
	self hasOnlySelfReturns ifTrue: [^self].
	parseTree addSelfReturn