running
tearDown
	super tearDown.
	
	(Smalltalk classNamed: #GTExampleOrganizer) ifNotNil: [ :class | 
		class instance reset. 
		class start ]