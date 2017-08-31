private
primDropRequestFileHandle: dropIndex
	"Primitive. Return the (read-only) file handle for some file that was just dropped onto the application.
	Fail if dropIndex is out of range or the primitive is not supported."
	<primitive: 'primitiveDropRequestFileHandle' module:'DropPlugin'>
	^nil