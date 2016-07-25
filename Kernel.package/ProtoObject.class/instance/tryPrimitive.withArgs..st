apply primitives
tryPrimitive: primIndex withArgs: argumentArray
	"This method is a template that the Smalltalk simulator uses to 
	execute primitives. See Object documentation whatIsAPrimitive."

	<primitive: 118 error: code >
	^ Context primitiveFailTokenFor: code