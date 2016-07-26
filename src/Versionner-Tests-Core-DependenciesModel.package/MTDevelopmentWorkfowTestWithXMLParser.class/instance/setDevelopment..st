utility
setDevelopment: aVersionString
	project configurationClass compile: 'development: spec
	<symbolicVersion: #''development''>

	spec for: #''common'' version: ''' , aVersionString , '''.'