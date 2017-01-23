asserting
assertNoDuplicatedAnnoucementsIn: anAnnouncement
	| duplications | 
	duplications := ((anAnnouncement subscriptions glmSubscriptions groupedBy: [ :aSubscription |
		aSubscription announcementClass ]) select: [ :each | each value size > 1 ]).
	self assert: duplications isEmpty description: 'The announcement should have only one subscription of a given type.'