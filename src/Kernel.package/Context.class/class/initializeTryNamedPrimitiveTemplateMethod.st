simulation
initializeTryNamedPrimitiveTemplateMethod
	| source method |
	source := 'tryNamedPrimitive
	"This method is a template that the Smalltalk simulator uses to 
	execute primitives. See Object documentation whatIsAPrimitive."
	<primitive:'' module:'' error: errorCode>
	^ Context primitiveFailTokenFor: errorCode'.
	method := Smalltalk compiler
				class: UndefinedObject;
				source: source;
				compile.
	TryNamedPrimitiveTemplateMethod := method copyWithSource: source.



	