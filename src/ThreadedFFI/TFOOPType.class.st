Class {
	#name : #TFOOPType,
	#superclass : #TFDerivedType,
	#category : #'ThreadedFFI-Types'
}

{ #category : #accessing }
TFOOPType >> basicType [
	
	^ TFBasicType pointer
]

{ #category : #marshalling }
TFOOPType >> emitMarshallFromPrimitive: anIRBuilder [ 
	
	anIRBuilder addTemp: #__marshall_temp.
	anIRBuilder storeTemp: #__marshall_temp.
	anIRBuilder popTop.
	anIRBuilder pushLiteral: PointerUtils.
	anIRBuilder pushTemp: #__marshall_temp.
	anIRBuilder send: #objectForOop:
]

{ #category : #marshalling }
TFOOPType >> emitMarshallToPrimitive: anIRBuilder [ 
	
	anIRBuilder addTemp: #__marshall_temp.
	anIRBuilder storeTemp: #__marshall_temp.
	anIRBuilder popTop.
	anIRBuilder pushLiteral: PointerUtils.
	anIRBuilder pushTemp: #__marshall_temp.
	anIRBuilder send: #oopForObject:
	
]

{ #category : #'reading-writing' }
TFOOPType >> readValue: anExternalAddress [
	^ self basicType readValue: anExternalAddress
]

{ #category : #'reading-writing' }
TFOOPType >> write: anObject into: anExternalAddress [ 
	
	self basicType write: anObject into: anExternalAddress
]
