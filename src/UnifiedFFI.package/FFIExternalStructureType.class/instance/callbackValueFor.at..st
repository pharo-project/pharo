accessing
callbackValueFor: anAlien at: index
	"This is the value for a callback.
	since the callbacks comunicates as Aliens, I receive the struct, but with an Alien 
	reference (which does not work for UFFI structires, they needs to be ExternalAddress)"
	self isPointer ifTrue: [ ^ anAlien pointerAt: index ].
	^ self objectClass fromHandle: (ExternalAddress fromAddress: anAlien address)