tests
testDate
	| date |
	date := Date year: 2012 month: 1 day: 1.
	self assert: (self materialize: 'Date[''2012-01-01'']') = date