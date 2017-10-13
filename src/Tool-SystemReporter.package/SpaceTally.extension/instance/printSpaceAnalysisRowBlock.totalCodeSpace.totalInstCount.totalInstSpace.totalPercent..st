*Tool-SystemReporter
printSpaceAnalysisRowBlock: printRow totalCodeSpace: totalPercent totalInstCount: totalInstCount totalInstSpace: totalCodeSpace totalPercent: totalInstSpace
	printRow
		valueWithArguments:
			{'Total'.
			totalCodeSpace printString.
			totalInstCount printString.
			totalInstSpace printString.
			(totalPercent printShowingDecimalPlaces: 1)}