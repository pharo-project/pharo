convenient accesses
traitNames
	"Retrieves the names of defined traits"

	^ self traits isDictionary
		ifTrue: [ self traits keys ]
		ifFalse:[ self traits collect:[ :trait| trait name ] ]