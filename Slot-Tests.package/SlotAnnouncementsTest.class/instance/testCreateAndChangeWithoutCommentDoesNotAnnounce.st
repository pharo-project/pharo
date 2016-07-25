tests-comments
testCreateAndChangeWithoutCommentDoesNotAnnounce

	self subscribeOn: ClassCommented.
	
	self make: [ :builder |
		builder 
			name: self aClassName ].
	
	self make: [ :builder | 
		builder 
			name: self aClassName;
			sharedPools: 'TestSharedPool' ].

	self assert: self collectedAnnouncements isEmpty.