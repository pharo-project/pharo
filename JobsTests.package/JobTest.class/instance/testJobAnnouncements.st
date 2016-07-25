tests
testJobAnnouncements
	
	| announcements |
	announcements := OrderedCollection new.

	Job jobAnnouncer weak when: JobStart send: #add: to: announcements.
	Job jobAnnouncer weak when: JobEnd send: #add: to: announcements.
	Job jobAnnouncer weak when: JobChange send: #add: to: announcements.

	self assert: announcements isEmpty.

	[ :job | job currentValue: 1 ] asJob run.	
	
	self 
		assert: (announcements collect: #class as: Array)
		equals: {JobStart. JobChange. JobEnd}.

	Job jobAnnouncer unsubscribe: announcements.