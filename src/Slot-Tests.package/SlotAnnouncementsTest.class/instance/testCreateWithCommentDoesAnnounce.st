tests-comments
testCreateWithCommentDoesAnnounce

	| classCommented |
	self subscribeOn: ClassCommented.
	
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			comment: 'A class Comment' ].
	
	self assert: self collectedAnnouncements size equals: 1.
	
	classCommented := self collectedAnnouncements anyOne.
	
	self assert: classCommented classCommented equals: aClass.
	self assert: classCommented oldComment equals: ''.
	self assert: classCommented newComment equals: 'A class Comment'.
	self assert: classCommented oldStamp equals: nil. "why nil?" 
	self assert: classCommented newStamp equals: nil. "why nil?"