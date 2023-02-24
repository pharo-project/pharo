"
! FFIBoolean32

Booleans in C are a convenience construction, and because of that each library chooses the definition of boolean they want. The most general convention is the one provided by *class:FFIBool*, and is equivalent to ushort (8bit), but time to time we need another.
 
The second most frequent boolean definition is the one provided by this class: uint32 (32bit).
"
Class {
	#name : #FFIBoolean32,
	#superclass : #FFIExternalType,
	#category : #'ThreadedFFI-Types'
}

{ #category : #accessing }
FFIBoolean32 class >> externalType [
	^ ExternalType bool
]

{ #category : #accessing }
FFIBoolean32 class >> externalTypeAlignment [
	^ 1
]

{ #category : #accessing }
FFIBoolean32 class >> externalTypeSize [
	^ 1
]

{ #category : #private }
FFIBoolean32 >> basicHandle: aHandle at: index [

	^ (aHandle signedLongAt: index) ~~ 0
]

{ #category : #private }
FFIBoolean32 >> basicHandle: aHandle at: index put: value [

	^ aHandle signedLongAt: index put: value asBit
]

{ #category : #accessing }
FFIBoolean32 >> defaultReturnOnError [

	^ false
]

{ #category : #'stack parameter classification' }
FFIBoolean32 >> stackValueParameterClass [
	^ #integer
]

{ #category : #accessing }
FFIBoolean32 >> tfExternalType [

	^ TFDerivedType boolean32
]
