instance creation
newWith: aClassCommented 
	^ self basicNew
		initializeWith: aClassCommented;
		yourself