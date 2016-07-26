tests-comments
testClassWithComment

	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			comment: 'A class Comment' ].
	
	self assert: aClass name = self aClassName.
	self assert: aClass comment = 'A class Comment'.
	
