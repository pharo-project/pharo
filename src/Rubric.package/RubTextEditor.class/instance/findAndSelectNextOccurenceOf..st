find-select
findAndSelectNextOccurenceOf: aRegex
	| where |
	where := self findNext: aRegex startingAt: self stopIndex.
	where ifNotNil: [self selectInterval: where].
	^ where