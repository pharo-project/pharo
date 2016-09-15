comparing
= other

	^ self class = other class and: [
		origin = other origin and: [ 
		target = other target and: [ 
		linkBuilder = other linkBuilder ] ] ]