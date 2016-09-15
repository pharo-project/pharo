visitor
visitMonticelloVersionsLoad: aMonticelloVersionLoaded 

	^	(self displayOperation: 'MC load'), 
		(self displayString: aMonticelloVersionLoaded versionNames asCommaStringAnd)