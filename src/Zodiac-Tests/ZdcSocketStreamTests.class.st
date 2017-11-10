"
ZdcSocketStreamTests tests ZdcSocketStream
"
Class {
	#name : #ZdcSocketStreamTests,
	#superclass : #ZdcAbstractSocketStreamTests,
	#category : #Zodiac-Tests
}

{ #category : #testing }
ZdcSocketStreamTests class >> isAbstract [ 
	^ false
]

{ #category : #accessing }
ZdcSocketStreamTests >> socketStreamClass [
	^ ZdcSocketStream 
]
