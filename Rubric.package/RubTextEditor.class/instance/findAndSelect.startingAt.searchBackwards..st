find-select
findAndSelect: aRegex startingAt: anIndex searchBackwards: searchBackwards
	| oldSelectionInterval where |
	self closeTypeIn.
	oldSelectionInterval := self selectionInterval.
	self selectInvisiblyFrom: anIndex to: anIndex - 1.
	where :=  searchBackwards
		ifTrue: [self findAndSelectPreviousOccurenceOf: aRegex]
		ifFalse: [self findAndSelectNextOccurenceOf: aRegex].
	where ifNil: [self selectInterval: oldSelectionInterval].
	^ where