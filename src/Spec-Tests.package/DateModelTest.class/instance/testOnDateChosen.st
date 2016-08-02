tests
testOnDateChosen
	| mookChoseDate |
	mookChoseDate := ChoseDate new.
	mookChoseDate
		calendar: StandardWindow new;
		date: Date tomorrow.
	testedInstance onDateChosen: mookChoseDate.
	self assert: testedInstance date equals: Date tomorrow