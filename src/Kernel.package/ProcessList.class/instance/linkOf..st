private
linkOf: anObject 
	^ self
		linkOf: anObject
		ifAbsent: [self error: 'No such element']