tests
testClassRecategorizationShouldAnnounceClassModified

	self subscribeOn: ClassRecategorized.
	
	aClass := self make: [ :builder|
		builder category: self aCategory ].	
	anotherClass := self make: [ :builder|
		builder category: self anotherCategory ].
	
	self assert: self collectedAnnouncements size equals: 1.
	announcement := self collectedAnnouncements first.	
	self assert: announcement oldCategory equals: self aCategory.
	self assert: announcement newCategory equals: self anotherCategory.
	self assert: announcement classRecategorized == anotherClass.