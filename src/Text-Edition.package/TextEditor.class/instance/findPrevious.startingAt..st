find-select
findPrevious: aRegex startingAt: searchIdx 
	| allRanges |
	allRanges := self findAll: aRegex endingAt: searchIdx.
	^ allRanges notEmpty
		ifTrue: [allRanges last].
