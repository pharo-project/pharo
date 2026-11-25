"
I define the TFFI type for FFIBoolean32
"
Class {
	#name : 'TFBoolean32Type',
	#superclass : 'TFBooleanType',
	#category : 'UnifiedFFI-Types',
	#package : 'UnifiedFFI',
	#tag : 'Types'
}

{ #category : 'accessing' }
TFBoolean32Type >> basicType [

	^ TFBasicType uint32
]
