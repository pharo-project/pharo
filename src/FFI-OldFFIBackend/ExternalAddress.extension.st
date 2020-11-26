Extension { #name : #ExternalAddress }

{ #category : #'*FFI-OldFFIBackend' }
ExternalAddress >> oldFree [
	"Primitive. Free the object pointed to on the external heap.
	Dangerous - may break your system if the receiver hasn't been
	allocated by ExternalAddress class>>allocate:. No checks are done."
	<primitive:'primitiveFFIFree' module:'SqueakFFIPrims'>
	^self primitiveFailed
]
