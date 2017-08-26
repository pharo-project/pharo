public
stopDefault
	"Stop and unregister the Default instance, if any, and clear it.
	Return the stopped instance, if any."
	
	| server |
	(server := default) ifNotNil: [ 
		default stop.  
		default := nil ].
	^ server