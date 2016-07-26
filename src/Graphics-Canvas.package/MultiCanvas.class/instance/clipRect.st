accessing
clipRect
	
	^super clipRect ifNil: [
		0@0 extent: 5000@5000
	].