testing
includesCategory: aString
	"Tests if a category is already included."
	
	^ self categories 
		ifNil: [ false ]
		ifNotNil: [ :categories | categories includes: aString ]