Extension { #name : #ByteArray }

{ #category : #'*Deprecated11' }
ByteArray >> nbFloat32AtOffset: zeroBasedOffset [

	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbFloat32AtOffset: `@arg'
			-> '`@receiver float32AtOffset: `@arg'.
	"Read 32-bit float at ZERO-based index."
	^ self float32AtOffset: zeroBasedOffset
]

{ #category : #'*Deprecated11' }
ByteArray >> nbFloat32AtOffset: zeroBasedOffset put: value [
	"Store 32-bit float at ZERO-based index."

	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbFloat32AtOffset: `@arg1 put: `@arg2'
			-> '`@receiver float32AtOffset: `@arg1  put: `@arg2'.
	^ self float32AtOffset: zeroBasedOffset put: value
]

{ #category : #'*Deprecated11' }
ByteArray >> nbFloat64AtOffset: zeroBasedOffset [
	"Read 64-bit float at ZERO-based index."
	
	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbFloat64AtOffset: `@arg'
			-> '`@receiver float64AtOffset: `@arg'.
	^ self float64AtOffset: zeroBasedOffset
]

{ #category : #'*Deprecated11' }
ByteArray >> nbFloat64AtOffset: zeroBasedOffset put: value [
	"Store 64-bit float at ZERO-based index."
	
	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbFloat64AtOffset: `@arg1 put: `@arg2'
			-> '`@receiver float64AtOffset: `@arg1 put: `@arg2'.
	^ self float64AtOffset: zeroBasedOffset put: value
]

{ #category : #'*Deprecated11' }
ByteArray >> nbInt16AtOffset: zeroBasedOffset [
	"Reads signed 16-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	
	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbInt16AtOffset: `@arg1'
			-> '`@receiver int16AtOffset: `@arg1'.
	^ self int16AtOffset: zeroBasedOffset
]

{ #category : #'*Deprecated11' }
ByteArray >> nbInt16AtOffset: zeroBasedOffset put: value [
	"Store signed 16-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order."
	
	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbInt16AtOffset: `@arg1 put: `@arg2'
			-> '`@receiver int16AtOffset: `@arg1 put: `@arg2'.
	^ self int16AtOffset: zeroBasedOffset put: value
]

{ #category : #'*Deprecated11' }
ByteArray >> nbInt32AtOffset: zeroBasedOffset [
	"Reads signed 32-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."

	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbInt32AtOffset: `@arg1'
			-> '`@receiver int32AtOffset: `@arg1'.
	^ self int32AtOffset: zeroBasedOffset
]

{ #category : #'*Deprecated11' }
ByteArray >> nbInt32AtOffset: zeroBasedOffset put: value [
	"Store signed 32-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order."
	
	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbInt32AtOffset: `@arg1 put: `@arg2'
			-> '`@receiver int32AtOffset: `@arg1 put: `@arg2'.
	^ self int32AtOffset: zeroBasedOffset put: value
]

{ #category : #'*Deprecated11' }
ByteArray >> nbInt64AtOffset: zeroBasedOffset [
	"Reads signed 64-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."

	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbInt64AtOffset: `@arg1'
			-> '`@receiver int64AtOffset: `@arg1'.
			
	^ self int64AtOffset: zeroBasedOffset
]

{ #category : #'*Deprecated11' }
ByteArray >> nbInt64AtOffset: zeroBasedOffset put: value [
	"Store signed 64-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order."

	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbInt64AtOffset: `@arg1 put: `@arg2'
			-> '`@receiver int64AtOffset: `@arg1 put: `@arg2'.
	^ self int64AtOffset: zeroBasedOffset put: value
]

{ #category : #'*Deprecated11' }
ByteArray >> nbInt8AtOffset: zeroBasedOffset [
	"Reads signed 8-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."

	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbInt8AtOffset: `@arg1'
			-> '`@receiver int8AtOffset: `@arg1'.
	^ self int8AtOffset: zeroBasedOffset
]

{ #category : #'*Deprecated11' }
ByteArray >> nbInt8AtOffset: zeroBasedOffset put: value [
	"Store signed 8-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order"

	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbInt8AtOffset: `@arg1 put: `@arg2'
			-> '`@receiver int8AtOffset: `@arg1 put: `@arg2'.
	^ self int8AtOffset: zeroBasedOffset put: value
]

{ #category : #'*Deprecated11' }
ByteArray >> nbLongAt: index [
	"Read signed long at one-based index"

	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith:
		'`@receiver nbLongAt: `@arg1' -> '`@receiver int32AtOffset: (`@arg1) -1'.
	^ self int32AtOffset: index - 1
]

{ #category : #'*Deprecated11' }
ByteArray >> nbLongAt: index put: value [
	"Store signed long at one-based index"

	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbLongAt: `@arg1 put `@arg2'
			-> '`@receiver int32AtOffset: (`@arg1) -1 put:`@arg2'.
	^ self int32AtOffset: index - 1 put: value
]

{ #category : #'*Deprecated11' }
ByteArray >> nbUInt16AtOffset: zeroBasedOffset [
	"Reads unsigned 16-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order"
	
	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbUInt16AtOffset: `@arg1'
			-> '`@receiver uint16AtOffset: `@arg1'.
	^ self uint16AtOffset: zeroBasedOffset
]

{ #category : #'*Deprecated11' }
ByteArray >> nbUInt16AtOffset: zeroBasedOffset put: value [
	"Store unsigned 16-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order"

	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbUInt16AtOffset: `@arg1 put: `@arg2'
			-> '`@receiver uint16AtOffset: `@arg1 put: `@arg2'.
	^ self uint16AtOffset: zeroBasedOffset put: value
]

{ #category : #'*Deprecated11' }
ByteArray >> nbUInt32AtOffset: zeroBasedOffset [
	"Reads unsigned 32-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbUInt32AtOffset: `@arg1'
			-> '`@receiver uint32AtOffset: `@arg1'.
	^ self uint32AtOffset: zeroBasedOffset
]

{ #category : #'*Deprecated11' }
ByteArray >> nbUInt32AtOffset: zeroBasedOffset put: value [
	"Store unsigned 32-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order."

	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbUInt32AtOffset: `@arg1 put: `@arg2'
			-> '`@receiver uint32AtOffset: `@arg1 put: `@arg2'.
	^ self uint32AtOffset: zeroBasedOffset put: value
]

{ #category : #'*Deprecated11' }
ByteArray >> nbUInt64AtOffset: zeroBasedOffset [
	"Reads unsigned 64-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	
	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbUInt64AtOffset: `@arg1'
			-> '`@receiver uint64AtOffset: `@arg1'.
	^ self uint64AtOffset: zeroBasedOffset
]

{ #category : #'*Deprecated11' }
ByteArray >> nbUInt64AtOffset: zeroBasedOffset put: value [
	"Store unsigned 64-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order"

	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbUInt64AtOffset: `@arg1 put: `@arg2'
			-> '`@receiver uint64AtOffset: `@arg1 put: `@arg2'.
	^ self uint64AtOffset: zeroBasedOffset put: value
]

{ #category : #'*Deprecated11' }
ByteArray >> nbUInt8AtOffset: zeroBasedOffset [
	"Reads unsigned 8-bit integer from memory at (receiver's address + ZERO-based offset) using native byte order."
	
	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbUInt8AtOffset: `@arg1'
			-> '`@receiver uint8AtOffset: `@arg1'.
	^ self uint8AtOffset: zeroBasedOffset
]

{ #category : #'*Deprecated11' }
ByteArray >> nbUInt8AtOffset: zeroBasedOffset put: value [
	"Store unsigned 8-bit integer into memory at (receiver's address + ZERO-based offset) using native byte order."
		
	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith: '`@receiver nbUInt8AtOffset: `@arg1 put: `@arg2'
			-> '`@receiver uint8AtOffset: `@arg1 put: `@arg2'.
	^ self uint8AtOffset: zeroBasedOffset put: value
]

{ #category : #'*Deprecated11' }
ByteArray >> nbUlongAt: index [
	"Read unsigned long at one-based index."

	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith:
		'`@receiver nbUlongAt: `@arg1'
		-> '`@receiver uint32AtOffset: `@arg1'.
	^ self uint32AtOffset: index
]

{ #category : #'*Deprecated11' }
ByteArray >> nbUlongAt: index put: value [
	"Store unsigned long at one-based index."
	
	self
		deprecated: 'All the native boosts(nb) should not be use anymore'
		transformWith:
		'`@receiver nbUlongAt: `@arg1 put: `@arg2'
		-> '`@receiver uint32AtOffset: `@arg1 put: `@arg2'.
	^ self uint32AtOffset: index put: value
]
