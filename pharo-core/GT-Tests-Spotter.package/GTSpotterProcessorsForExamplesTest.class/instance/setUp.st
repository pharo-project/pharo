running
setUp
	super setUp.
	
	(Smalltalk classNamed: #GTExampleOrganizer) ifNotNil: [ :class | 
		class instance reset. 
		class stop ]