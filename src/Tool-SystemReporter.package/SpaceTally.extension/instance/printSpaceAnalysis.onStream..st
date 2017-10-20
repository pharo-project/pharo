*Tool-SystemReporter
printSpaceAnalysis: threshold onStream: stream
	"If threshold > 0, then only those classes with more than that number
	of instances will be shown, and they will be sorted by total instance space.
	If threshold = 0, then all classes will appear, sorted by name."

	| totalCodeSpace totalInstCount totalInstSpace totalPercent classNameLength printRow |
	self systemWideSpaceTally.
	totalCodeSpace := totalInstCount := totalInstSpace := 0.
	classNameLength := 1.
	results
		do: [ :each | 
			classNameLength := classNameLength max: each analyzedClassName size.
			totalCodeSpace := totalCodeSpace + each codeSize.
			totalInstCount := totalInstCount + each instanceCount.
			totalInstSpace := totalInstSpace + each spaceForInstances ].
	totalPercent := 0.0.
	printRow := self
		printSpaceAnalysisRowBlock: classNameLength
		onStream: stream.
	stream timeStamp.
	printRow
		valueWithArguments: {'Class' . 'code space' . '# instances' . 'inst space' . 'percent'}.
	threshold > 0
		ifTrue: [ "If inst count threshold > 0, then sort by space"
			results := results
				select: [ :s | 
					s instanceCount >= threshold
						or: [ s spaceForInstances > (totalInstSpace // 500) ] ].
			results
				sort: [ :s :s2 | s spaceForInstances > s2 spaceForInstances ] ].
	results
		do: [ :s | 
			| percent |
			percent := s spaceForInstances * 100.0 / totalInstSpace.
			totalPercent := totalPercent + percent.
			printRow
				valueWithArguments:
					{s analyzedClassName.
					s codeSize printString.
					s instanceCount printString.
					s spaceForInstances humanReadableSIByteSize.
					(percent printShowingDecimalPlaces: 1)} ].
	stream cr.
	self
		printSpaceAnalysisRowBlock: printRow
		totalCodeSpace: totalPercent
		totalInstCount: totalInstCount
		totalInstSpace: totalCodeSpace
		totalPercent: totalInstSpace