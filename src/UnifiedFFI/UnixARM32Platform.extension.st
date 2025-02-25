Extension { #name : 'UnixARM32Platform' }

{ #category : '*UnifiedFFI' }
UnixARM32Platform >> ffiFloat64Alignment [

	"64 bits value are aligned to 8 bytes"
	^ 8
]

{ #category : '*UnifiedFFI' }
UnixARM32Platform >> ffiInt64Alignment [

	"64 bits value are aligned to 8 bytes"
	^ 8
]
