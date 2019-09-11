"
ZdcSimpleSocketStreamTests tests ZdcSimpleSocketStream
"
Class {
	#name : #ZdcSimpleSocketStreamTest,
	#superclass : #ZdcAbstractSocketStreamTest,
	#category : #'Zodiac-Tests'
}

{ #category : #testing }
ZdcSimpleSocketStreamTest class >> isAbstract [ 
	^ false
]

{ #category : #accessing }
ZdcSimpleSocketStreamTest >> socketStreamClass [
	^ ZdcSimpleSocketStream 
]
