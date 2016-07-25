instance-creation
filterWith: aRegex dataSource: aDataSource
	^ self new
		pattern: aRegex;
		dataSource: aDataSource;
		filter