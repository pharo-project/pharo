testing
isWritable
	"Literal variables are writable, if they aren't global bindings for class names, like
 #Object -> Object "

	^ (self isGlobalVariable and: [ self isGlobalClassNameBinding ]) not