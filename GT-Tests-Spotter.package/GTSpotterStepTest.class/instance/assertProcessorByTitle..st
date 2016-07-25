convenience
assertProcessorByTitle: aString
	^ self assertProcessorMatching: [ :processor | processor title = aString ]