find-select
findAndSelectPreviousOccurenceOf: aRegex
	| where |
	where := self findPrevious: aRegex startingAt: self startIndex.
	where isNil ifFalse: [self selectInterval: where].
	^ where