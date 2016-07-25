accessing
peekFor: object
	"Answer false and do not move over the next element if it is not equal to object, or if the receiver is at the end. 
	Answer true and move over the next element when it is equal to object."

	^ self peek = object
		ifTrue: [ 
			self next.
			true ]
		ifFalse: [ false ]