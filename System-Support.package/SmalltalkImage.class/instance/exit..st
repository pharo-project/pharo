snapshot and quit
exit: exitStatus
	"Primitive. Exit to another operating system on the host machine, if one
	exists. All state changes in the object space since the last snapshot are lost.
	Essential. See Object documentation whatIsAPrimitive.
	
	Possible values for exitStatus:
	0:   success
	> 1: error"

	<primitive: 113>
	self primitiveFailed