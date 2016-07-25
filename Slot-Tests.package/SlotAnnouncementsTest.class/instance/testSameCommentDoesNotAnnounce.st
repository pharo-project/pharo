tests-comments
testSameCommentDoesNotAnnounce

	self subscribeOn: ClassCommented.
	
	self make: [ :builder |
		builder 
			name: self aClassName;
			comment: 'A class Comment' ].

	self make: [ :builder |
		builder 
			name: self aClassName;
			comment: 'A class Comment' ].
	
	self assert: self collectedAnnouncements size equals: 1.