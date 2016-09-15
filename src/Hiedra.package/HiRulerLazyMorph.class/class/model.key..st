instance creation
model: aModel key: anObject 
	^ self basicNew
		initializeWithModel: aModel 
		key: anObject;
		yourself