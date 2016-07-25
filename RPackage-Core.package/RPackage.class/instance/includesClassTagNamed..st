testing
includesClassTagNamed: aString
	^ self classTags anySatisfy: [ :each | each name = aString ]