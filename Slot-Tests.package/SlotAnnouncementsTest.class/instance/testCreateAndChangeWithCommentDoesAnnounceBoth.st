tests-comments
testCreateAndChangeWithCommentDoesAnnounceBoth

	| classCommented |
	self subscribeOn: ClassCommented.
	
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			comment: 'A class Comment' ].
	
	aClass := self make: [ :builder | 
		builder 
			name: self aClassName;
			comment: 'Another class Comment' ].

	self assert: self collectedAnnouncements size equals: 2.
	
	classCommented := self collectedAnnouncements first.
	self assert: classCommented classCommented equals: aClass.
	self assert: classCommented oldComment equals: ''.
	self assert: classCommented newComment equals: 'A class Comment'.
	self assert: classCommented oldStamp equals: nil. "why nil?" 
	self assert: classCommented newStamp equals: nil. "why nil?"
	
	classCommented := self collectedAnnouncements second.
	self assert: classCommented classCommented equals: aClass.
	self assert: classCommented oldComment equals: 'A class Comment'.
	self assert: classCommented newComment equals: 'Another class Comment'.
	self assert: classCommented oldStamp equals: nil. "why nil?" 
	self assert: classCommented newStamp equals: nil. "why nil?"