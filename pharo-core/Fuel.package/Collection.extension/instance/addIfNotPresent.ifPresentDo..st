*Fuel
addIfNotPresent: anObject ifPresentDo: aBlock
	"Include anObject as one of the receiver's elements and then value aBlock, but only if there
	is no such element already. Anwser anObject."

	(self includes: anObject) 
		ifFalse: [ self add: anObject ]
		ifTrue: [ aBlock value ].
	^ anObject