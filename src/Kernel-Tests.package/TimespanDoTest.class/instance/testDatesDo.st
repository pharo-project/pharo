tests
testDatesDo
	| dateArray |
	dateArray := OrderedCollection new.
	7
		to: 97
		do: [:each | dateArray
				addLast: (Date year: 2003 day: each)].
	dateArray := dateArray asArray.
	self assert: aTimespan dates = dateArray