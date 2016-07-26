storing
basicStoreVersion: aVersion
	"we store a version if it is cacheable and we don't have the file already"
	
	(aVersion isCacheable and: [(self includesFileNamed: aVersion fileName) not])
		ifTrue: [super basicStoreVersion: aVersion]
