find-select
findNextString: aSubstring startingAt: searchIdx 
	| idx |
	idx := self string findString: aSubstring startingAt: searchIdx.
	^ idx isZero ifFalse: [idx to: idx + aSubstring size - 1]