running
tearDown
	nautilus ifNotNil: [ :aNautilus | aNautilus ui ifNotNil: [ :aUI | aUI close ] ].
	lastValue ifNotNil: [ :aValue | SharedPackageFilterPatternsHolder instance lastValue: aValue ].
	super tearDown