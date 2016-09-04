private
primDropRequestFileName: dropIndex
	"Primitive. Return the file name for some file that was just dropped onto the application.
	Fail if dropIndex is out of range or the primitive is not supported."
	<primitive: 'primitiveDropRequestFileName' module:'DropPlugin'>
	^nil