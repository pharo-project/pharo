tests-comments
testClassWithCommentAndStamp

	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			comment: 'A class Comment' stamp: (Object>>#halt) stamp ].
	
	self assert: aClass name = self aClassName.
	self assert: aClass comment = 'A class Comment'.
	self assert: aClass organization commentStamp = (Object>>#halt) stamp.
	
