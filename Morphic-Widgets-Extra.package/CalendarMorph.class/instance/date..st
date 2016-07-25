accessing
date: aDate

	| recompute |
	recompute := date isNil or: [date month ~= aDate month].
	date := aDate.
	recompute
		ifTrue: [self computeDays]