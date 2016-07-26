accessing
classToUse
	^ self selectedClass
		ifNotNil: [self classIsMeta 
				ifTrue: [self selectedClass class]
				ifFalse: [self selectedClass]]