reflective operations
basicIdentityHash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	This method must not be overridden, except by SmallInteger.
	Primitive. Fails if the receiver is a SmallInteger. Essential.
	See Object documentation whatIsAPrimitive.

	Do not override. Use #identityHash unless you really know what you're doing.'"

	<primitive: 75>
	self primitiveFailed