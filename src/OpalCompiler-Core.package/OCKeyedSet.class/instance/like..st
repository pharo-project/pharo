adding
like: anObject
	"Answer an object in the receiver that is equal to anObject,
	nil if no such object is found. Relies heavily on hash properties"

	| index |

	^(index := self scanFor: (keyBlock value: anObject)) = 0
		ifFalse: [array at: index]
		ifTrue: [nil]