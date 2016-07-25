options
port
	"Return the integer port number we are (or will be) listening on"
	
	^ self optionAt: #port ifAbsent: [ 1701 ] 