ui
openOn: configuration
	^ self new
		configuration: configuration;
		createBaselineIfEmptyConfiguration;
		openWithSpec;
		yourself