private
paragraph
	^ morph 
		ifNotNil: [morph paragraph]
		ifNil: [self error: 'paragraph is not available']