accessing
min: aNumber
	self migrateProgressWhileUpdatingBounds: [ min := aNumber ].
	self announceChange.