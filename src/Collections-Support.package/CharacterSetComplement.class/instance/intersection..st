enumerating
intersection: anotherSet
	"if anotherSet is small, let it take care of the operation. 
	Otherwise, invoke de Morgan's law."
	anotherSet abstractSize < self abstractSize
		ifTrue: [ ^ anotherSet intersectSmallWithLarge: self ].
	^ self class of: (self complement union: anotherSet complement)