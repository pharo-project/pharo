"
I define the TFFI type for FFIBoolean32
"
Class {
	#name : 'TFBoolean32Type',
	#superclass : 'TFBooleanType',
	#category : 'ThreadedFFI-Types',
	#package : 'ThreadedFFI',
	#tag : 'Types'
}

{ #category : 'accessing' }
TFBoolean32Type >> basicType [

	^ TFBasicType uint32
]
