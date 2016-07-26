accessing
isLeapYear: aYearInteger


	| adjustedYear |
	adjustedYear := aYearInteger > 0
		ifTrue: [aYearInteger]
		ifFalse: [(aYearInteger + 1) negated].

	"There was no year 0"
	^ ((adjustedYear \\ 4 ~= 0) or: [(adjustedYear \\ 100 = 0) and: [adjustedYear \\ 400 ~= 0]]) not.