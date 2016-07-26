initialization
initializeClass: aClass
	self initialize.
	self elements: ((aClass selectors sorted: [ :a :b | a < b ])
		collect: [ :each | FTExampleMethodModel method: aClass>>each ])