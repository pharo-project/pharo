Extension { #name : #Float }

{ #category : #'*UnifiedFFI' }
Float >> packAsDoubleToArity: arity [ 
	| rolledPointer |
		
	rolledPointer := self.
	1 to: arity do: [ :index | rolledPointer := rolledPointer pointerDouble ].
	^ rolledPointer	
]

{ #category : #'*UnifiedFFI' }
Float >> packAsFloatToArity: arity [ 
	| rolledPointer |
		
	rolledPointer := self.
	1 to: arity do: [ :index | rolledPointer := rolledPointer pointerFloat ].
	^ rolledPointer	
]

{ #category : #'*UnifiedFFI' }
Float >> packToArity: arity [ 
	self error: 'Use #rollAsFloatArity: or #rollAsDoubleArity:'
]

{ #category : #'*UnifiedFFI' }
Float >> pointer [ 
	"Floats in Pharo are 64bits (8 bytes) and that correspond directly with a double on C.
	 So 'default' behaviour will be to address it as a double (float is also available in case is 
	 needed)"
	^ self pointerDouble
]

{ #category : #'*UnifiedFFI' }
Float >> pointerDouble [
	"sizeof(double)== 8"
	^ (ByteArray new: FFIFloat64 externalTypeSize)
		doubleAt: 1 put: self;
		yourself
]

{ #category : #'*UnifiedFFI' }
Float >> pointerFloat [
	"sizeof(float)== 4"
	^ (ByteArray new: FFIFloat32 externalTypeSize)
		floatAt: 1 put: self;
		yourself
]
