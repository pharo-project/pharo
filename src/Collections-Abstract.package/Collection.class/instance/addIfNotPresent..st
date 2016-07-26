adding
addIfNotPresent: anObject
	"Include anObject as one of the receiver's elements, but only if there
	is no such element already. Anwser anObject."

	(self includes: anObject) ifFalse: [self add: anObject].
	^ anObject