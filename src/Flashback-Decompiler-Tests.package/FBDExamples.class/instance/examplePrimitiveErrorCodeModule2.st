examples - pragmas
examplePrimitiveErrorCodeModule2
	"Primitive. Attempt to load a module of the given name.
	Fail if module cannot be found, or cannot be loaded,
	or failed to initialize"

	<primitive: 'primLoadModule'  module: '' error: errorCode >
	^ self primitiveFailed