Extension { #name : #ByteArray }

{ #category : #'*FFI-OldFFIBackend' }
ByteArray >> oldFFIPrimFloat32At: byteOffset [
	<primitive:'primitiveFFIFloatAt' module:'SqueakFFIPrims'>
	^self primitiveFailed
]

{ #category : #'*FFI-OldFFIBackend' }
ByteArray >> oldFFIPrimFloat32At: byteOffset put: value [
	<primitive:'primitiveFFIFloatAtPut' module:'SqueakFFIPrims'>
	self isReadOnlyObject 
		ifTrue: [ ^ self modificationForbiddenFor: #oldFFIPrimFloat32At:put: index: byteOffset value: value ].
	^self primitiveFailed
]

{ #category : #'*FFI-OldFFIBackend' }
ByteArray >> oldFFIPrimFloat64At: byteOffset [
	<primitive:'primitiveFFIDoubleAt' module:'SqueakFFIPrims'>
	^self primitiveFailed
]

{ #category : #'*FFI-OldFFIBackend' }
ByteArray >> oldFFIPrimFloat64At: byteOffset put: value [
	<primitive:'primitiveFFIDoubleAtPut' module:'SqueakFFIPrims'>
	self isReadOnlyObject 
		ifTrue: [ ^ self modificationForbiddenFor: #oldFFIPrimFloat64At:put: index: byteOffset value: value ].
	^self primitiveFailed
]

{ #category : #'*FFI-OldFFIBackend' }
ByteArray >> oldIntegerAt: byteOffset put: value size: nBytes signed: aBoolean [
	"Primitive. Store the given value as integer of nBytes size
	in the receiver. Fail if the value is out of range.
	Note: This primitive will access memory in the outer space if
	invoked from ExternalAddress."
	<primitive: 'primitiveFFIIntegerAtPut' module:'SqueakFFIPrims'>
	^self primitiveFailed
]

{ #category : #'*FFI-OldFFIBackend' }
ByteArray >> oldIntegerAt: byteOffset size: nBytes signed: aBoolean [
	"Primitive. Return an integer of nBytes size from the receiver.
	Note: This primitive will access memory in the outer space if
	invoked from ExternalAddress."
	<primitive: 'primitiveFFIIntegerAt' module:'SqueakFFIPrims'>
	^self primitiveFailed
]
