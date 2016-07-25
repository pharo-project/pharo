find-select
findAndSelectNextOccurenceOf: aRegex
	| where |
	where := self findNext: aRegex startingAt: self stopIndex.
	where ifNotNil: [self selectInterval: where].
	"(morph respondsTo: #editView) ifTrue: [morph editView selectionInterval: self selectionInterval]."
	^ where