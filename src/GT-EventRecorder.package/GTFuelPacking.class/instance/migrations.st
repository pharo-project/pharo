accessing
migrations
	migrations ifNil: [ self initializeMigrations ].
	^ migrations