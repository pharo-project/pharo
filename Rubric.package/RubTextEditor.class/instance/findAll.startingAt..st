find-select
findAll: aRegex startingAt: searchIdx 
	| ranges |
	ranges := aRegex matchingRangesIn: (self string copyFrom: searchIdx to: self string size).
	^ ranges collect: [:i | i + searchIdx - 1]
			