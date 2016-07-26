tests
testNoDuplicateRegistrationOfAnnouncementsOnDataSource
	| table amountOfMenuItemSelectedSubscriptions |
	window := GLMCompositePresentation new 
			with: [ :c | 
				c fastList selectionAct: [ 42 inspect ] entitled: 'Answer' ];
			openOn: (1 to: 42).
	table := self find: FTTableMorph in: window.
	amountOfMenuItemSelectedSubscriptions := table dataSource announcer subscriptions glmSubscriptions count: [ :each | each announcementClass = GLMMenuItemSelected ].
	self assert: amountOfMenuItemSelectedSubscriptions equals: 1