"
I'm a 128bits (cuadruple precision) float. 
It is usually not used, but some compiler modes support it (__float128 in gcc)

THIS IS NOT YET SUPPORTED
"
Class {
	#name : #FFIFloat128,
	#superclass : #FFIFloatType,
	#category : #'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIFloat128 class >> externalType [ 
	^ self error: '128bits floats are not currently supported!'
]

{ #category : #accessing }
FFIFloat128 class >> externalTypeAlignment [
	^ 16
]

{ #category : #accessing }
FFIFloat128 class >> externalTypeSize [
	^ 16
]

{ #category : #private }
FFIFloat128 >> basicHandle: aHandle at: index [
	self error: 'Not yet supported!'
]

{ #category : #private }
FFIFloat128 >> basicHandle: aHandle at: index put: value [
	self error: 'Not yet supported!'
]
