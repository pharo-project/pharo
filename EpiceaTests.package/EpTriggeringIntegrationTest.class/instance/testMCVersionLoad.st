tests
testMCVersionLoad
	
	| gofer anEntry |
	gofer := Gofer new.
	gofer disablePackageCache.
	gofer 
		repository: self monticelloRepository;
		package: 'GoferFoo'.
	gofer load.
	gofer unload.

	"Check EpMonitor logged it correctly"
	self assert: (self countLogEventsWith: EpClassAddition) equals: 1.

	anEntry := (self allLogEntriesWith: EpClassAddition) anyOne.

	self 
		assert: (anEntry tags includesKey: EpLog triggererReferenceKey) 
		description: 'It did not recognize that the code change was triggered by other event.'