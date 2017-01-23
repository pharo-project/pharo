tests
testNoDuplicateRegistrationsInFastTableRenderer
	| table |
	window := GLMCompositePresentation new
	  with: [ :c |
	    c fastList ];
	   openOn: (1 to: 42).

	table := self find: FTTableMorph in: window.
	self assertNoDuplicatedAnnoucementsIn: table announcer.
	self assertNoDuplicatedAnnoucementsIn: table dataSource announcer.