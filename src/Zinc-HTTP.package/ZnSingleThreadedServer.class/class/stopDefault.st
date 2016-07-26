public
stopDefault
	"Stop and unregister the Default instance, if any, and clear it.
	Return the stopped instance, if any."
	
	| server |
	(server := Default) ifNotNil: [ 
		Default stop.  
		Default := nil ].
	^ server