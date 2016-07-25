tests-comments
testEmptyCommentDoesNotAnnounce

	self subscribeOn: ClassCommented.
	
	self make: [ :builder |
		builder 
			name: self aClassName;
			comment: '' ].

	self make: [ :builder |
		builder 
			name: self aClassName;
			comment: '';
			sharedPools: 'TestSharedPool' ].

	self assert: self collectedAnnouncements isEmpty.