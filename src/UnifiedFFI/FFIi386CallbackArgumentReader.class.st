Class {
	#name : #FFIi386CallbackArgumentReader,
	#superclass : #FFICallbackArgumentReader,
	#category : #'UnifiedFFI-Architecture'
}

{ #category : #types }
FFIi386CallbackArgumentReader >> stackIntegerType [
	
	^ FFIInt32 new
]

{ #category : #types }
FFIi386CallbackArgumentReader >> stackUnsignedIntegerType [
	
	^ FFIUInt32 new
]
