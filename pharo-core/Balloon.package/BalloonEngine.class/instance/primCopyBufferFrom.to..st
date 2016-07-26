primitives-misc
primCopyBufferFrom: oldBuffer to: newBuffer
	"Copy the contents of oldBuffer into the (larger) newBuffer"
	<primitive: 'primitiveCopyBuffer' module: 'B2DPlugin'>
	^self primitiveFailed