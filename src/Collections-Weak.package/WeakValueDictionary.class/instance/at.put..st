accessing
at: key put: anObject 
	"Set the value at key to be anObject.  If key is not found, create a new
	entry for key and set is value to anObject. Answer anObject."
	| index element |
	index := self findElementOrNil: key.
	element := array at: index.
	element == nil
		ifTrue: [self atNewIndex: index put: (WeakValueAssociation key: key value: anObject)]
		ifFalse: [element value: anObject].
	^ anObject