Class {
	#name : #TFAbstractType,
	#superclass : #FFIExternalReference,
	#category : #'ThreadedFFI-Types'
}

{ #category : #size }
TFAbstractType >> byteSize [
	
	^ self subclassResponsibility
]

{ #category : #reading }
TFAbstractType >> callbackReadValue: anExternalAddress [

	^ self readValue: anExternalAddress offset: 1
]

{ #category : #writing }
TFAbstractType >> callbackWrite: aNumber into: anExternalAddress [

	self write: aNumber into: anExternalAddress
]

{ #category : #accessing }
TFAbstractType >> isValid [ 

	^ handle isNull not
]

{ #category : #reading }
TFAbstractType >> readReturnValue: anExternalAddress [

	^ self subclassResponsibility
]

{ #category : #reading }
TFAbstractType >> readValue: anExternalAddress [

	^ self readValue: anExternalAddress offset: 1
]

{ #category : #reading }
TFAbstractType >> readValue: anExternalAddress offset: offset [

	^ self subclassResponsibility
]

{ #category : #private }
TFAbstractType >> validate [
	
	^ self subclassResponsibility
]

{ #category : #writing }
TFAbstractType >> write: aNumber into: anExternalAddress [

	self subclassResponsibility
]
