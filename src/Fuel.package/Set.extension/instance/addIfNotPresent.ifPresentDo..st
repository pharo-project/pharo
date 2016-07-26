*Fuel
addIfNotPresent: anObject ifPresentDo: aBlock
	"Include anObject as one of the receiver's elements and then value aBlock, but only if there
	is no such element already. Anwser anObject."

	| index |
	index := self scanFor: anObject.
	(array at: index) 
		ifNil: [self atNewIndex: index put: anObject asSetElement]
		ifNotNil: [ aBlock value ].
	^ anObject

	"Sets in Pharo 1.2 can't' contain nil, so this implementation is wrong in that version. This is the right one (in Pharo 1.2):

	| index |
	anObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index := self findElementOrNil: anObject.
	(array at: index) 
		ifNil: [ self atNewIndex: index put: anObject ]
		ifNotNil: [ aBlock value ].
	^ anObject"