find-select
findAll: aRegex endingAt: searchIdx 
	| ranges |
	ranges := aRegex matchingRangesIn: (self string copyFrom: 1 to: searchIdx).
	^ ranges 
			