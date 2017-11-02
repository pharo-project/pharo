Extension { #name : #ByteArray }

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbFloat32AtOffset: zeroBasedOffset [
	"Read 32-bit float at ZERO-based index."
	^ self floatAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbFloat32AtOffset: zeroBasedOffset put: value [
	"Store 32-bit float at ZERO-based index."
	^ self floatAt: zeroBasedOffset + 1 put: value
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbFloat64AtOffset: zeroBasedOffset [
	"Read 64-bit float at ZERO-based index."
	^ self doubleAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbFloat64AtOffset: zeroBasedOffset put: value [
	"Store 64-bit float at ZERO-based index."
	^ self doubleAt: zeroBasedOffset + 1 put: value
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbInt16AtOffset: zeroBasedOffset [
	"Reads signed 16-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self signedShortAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbInt16AtOffset: zeroBasedOffset put: value [
	"Store signed 16-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self signedShortAt: zeroBasedOffset + 1 put: value
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbInt32AtOffset: zeroBasedOffset [
	"Reads signed 32-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self signedLongAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbInt32AtOffset: zeroBasedOffset put: value [
	"Store signed 32-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self signedLongAt: zeroBasedOffset + 1 put: value
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbInt64AtOffset: zeroBasedOffset [
	"Reads signed 64-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self signedLongAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbInt64AtOffset: zeroBasedOffset put: value [
	"Store signed 64-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self signedLongAt: zeroBasedOffset + 1 put: value
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbInt8AtOffset: zeroBasedOffset [
	"Reads signed 8-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self signedByteAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbInt8AtOffset: zeroBasedOffset put: value [
	"Store signed 8-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order"
	^ self signedByteAt: zeroBasedOffset + 1 put: value
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbLongAt: index [
	"Read signed long at one-based index"
	^ self signedLongAt: index
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbLongAt: index put: value [
	"Store signed long at one-based index"
	^ self signedLongAt: index put: value
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbUInt16AtOffset: zeroBasedOffset [
	"Reads unsigned 16-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order"
	^ self unsignedShortAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbUInt16AtOffset: zeroBasedOffset put: value [
	"Store unsigned 16-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order"
	^ self unsignedShortAt: zeroBasedOffset + 1 put: value
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbUInt32AtOffset: zeroBasedOffset [
	"Reads unsigned 32-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self unsignedLongAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbUInt32AtOffset: zeroBasedOffset  put: value [
	"Store unsigned 32-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self unsignedLongAt: zeroBasedOffset + 1 put: value
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbUInt64AtOffset: zeroBasedOffset [
	"Reads unsigned 64-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self unsignedLongLongAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbUInt64AtOffset: zeroBasedOffset put: value [
	"Store unsigned 64-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order"
	^ self unsignedLongLongAt: zeroBasedOffset + 1 put: value
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbUInt8AtOffset: zeroBasedOffset [
	"Reads unsigned 8-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self unsignedByteAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbUInt8AtOffset: zeroBasedOffset put: value [
	"Store unsigned 8-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order."
	^ self unsignedByteAt: zeroBasedOffset + 1 put: value
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbUlongAt: index [
	"Read unsigned long at one-based index."
	^ self unsignedLongAt: index
]

{ #category : #'*UnifiedFFI-Legacy' }
ByteArray >> nbUlongAt: index put: value [
	"Store unsigned long at one-based index."
	^ self unsignedLongAt: index put: value	
]
