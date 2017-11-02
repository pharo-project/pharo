Extension { #name : #OSPlatform }

{ #category : #'*UnifiedFFI' }
OSPlatform >> ffiCallingConvention [
	^ #cdecl
]

{ #category : #'*UnifiedFFI' }
OSPlatform >> ffiFloat64Alignment [
	"By default is 4, windows and maybe others can use other"
	^ Smalltalk wordSize
]

{ #category : #'*UnifiedFFI' }
OSPlatform >> ffiInt64Alignment [
	"By default is 4, windows and maybe others can use other"
	^ Smalltalk wordSize
]

{ #category : #'*UnifiedFFI' }
OSPlatform >> ffiLongAlignment [
	"By default is 4, windows and maybe others can use other"
	^ Smalltalk wordSize
]

{ #category : #'*UnifiedFFI' }
OSPlatform >> ffiModuleName: aLibrary [ 
	^ self subclassResponsibility
]

{ #category : #'*UnifiedFFI' }
OSPlatform >> ffiPointerAlignment [
	"By default is 4, windows and maybe others can use other"
	^ Smalltalk wordSize
]
