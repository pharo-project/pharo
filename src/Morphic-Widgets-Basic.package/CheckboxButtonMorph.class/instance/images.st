accessing
images
	^ images ifNil: [images := Dictionary newFromPairs: {
								#off . [self offImage] .
								#pressed . [self pressedImage] . 
								#on . [self onImage] .
								#repressed . [self repressedImage ifNil: [self onImage]] }]