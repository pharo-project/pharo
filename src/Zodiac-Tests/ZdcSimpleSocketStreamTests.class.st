"
ZdcSimpleSocketStreamTests tests ZdcSimpleSocketStream
"
Class {
	#name : #ZdcSimpleSocketStreamTests,
	#superclass : #ZdcAbstractSocketStreamTests,
	#category : #Zodiac-Tests
}

{ #category : #testing }
ZdcSimpleSocketStreamTests class >> isAbstract [ 
	^ false
]

{ #category : #accessing }
ZdcSimpleSocketStreamTests >> socketStreamClass [
	^ ZdcSimpleSocketStream 
]
