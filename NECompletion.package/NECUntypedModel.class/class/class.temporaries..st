instance creation
class: aClass temporaries: aCollection 
	^self 
		class: aClass
		temporaries: aCollection
		additionals: #()
		variables: true
		selectors: true