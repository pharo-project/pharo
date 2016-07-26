accessing
enabled
	"Answer if HTTPProgress should be signalled.
	The default is false."
	
	^ self value
		ifNil: [ false ]
		ifNotNil: [ :value | value ]