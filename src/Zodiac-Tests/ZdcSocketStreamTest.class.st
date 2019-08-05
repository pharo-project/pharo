"
ZdcSocketStreamTests tests ZdcSocketStream
"
Class {
	#name : #ZdcSocketStreamTest,
	#superclass : #ZdcAbstractSocketStreamTest,
	#category : #'Zodiac-Tests'
}

{ #category : #testing }
ZdcSocketStreamTest class >> isAbstract [ 
	^ false
]

{ #category : #accessing }
ZdcSocketStreamTest >> socketStreamClass [
	^ ZdcSocketStream 
]
