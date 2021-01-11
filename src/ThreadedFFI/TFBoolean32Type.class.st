"
I define the TFFI type for FFIBoolean32
"
Class {
	#name : #TFBoolean32Type,
	#superclass : #TFBooleanType,
	#category : #'ThreadedFFI-Types'
}

{ #category : #accessing }
TFBoolean32Type >> basicType [
	
	^ TFBasicType uint32
]
