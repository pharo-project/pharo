accessing-query
queryKeys
	"Return the collection of all query keys in the receiver"
	
	^ self hasQuery
		ifTrue: [ self query keys ]
		ifFalse: [ #() ]