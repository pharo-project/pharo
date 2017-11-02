"
I'm a 16bits (half precision) float. 
It is usually not used, but ARM processors support them (__fp16 in gcc). 

THIS IS NOT YET SUPPORTED
"
Class {
	#name : #FFIFloat16,
	#superclass : #FFIFloatType,
	#category : 'UnifiedFFI-Types'
}

{ #category : #accessing }
FFIFloat16 class >> externalType [
	self error: '16bits floats are not currently supported!'
]

{ #category : #accessing }
FFIFloat16 class >> externalTypeAlignment [
	^ 2
]

{ #category : #accessing }
FFIFloat16 class >> externalTypeSize [
	^ 2
]

{ #category : #private }
FFIFloat16 >> basicHandle: aHandle at: index [
	self error: 'Not yet supported!'
]

{ #category : #private }
FFIFloat16 >> basicHandle: aHandle at: index put: value [
	self error: 'Not yet supported!'
]
