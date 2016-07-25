tests-integration
testClassAddedToNewCategoryShouldAnnounceCategoryAdded

	self subscribeOn: CategoryAdded.	

	self deny: (self class environment organization includesCategory: self aCategory).
	
	Object 
		subclass: self aClassName
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self aCategory.

	self assert: (self class environment organization includesCategory: self aCategory).

	self assert: self collectedAnnouncements size equals: 1.