user interface
index
	^ self list
		indexOf: self realValue
		ifAbsent: [self realValue
				ifNil: [0]
				ifNotNil: [self addToList: self realValue.
					self list size]]