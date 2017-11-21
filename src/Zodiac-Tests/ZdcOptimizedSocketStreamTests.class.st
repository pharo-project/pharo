"
ZdcOptimizedSocketStreamTests tests ZdcOptimizedSocketStream
"
Class {
	#name : #ZdcOptimizedSocketStreamTests,
	#superclass : #ZdcAbstractSocketStreamTests,
	#category : #Zodiac-Tests
}

{ #category : #testing }
ZdcOptimizedSocketStreamTests class >> isAbstract [ 
	^ false
]

{ #category : #accessing }
ZdcOptimizedSocketStreamTests >> socketStreamClass [
	^ ZdcOptimizedSocketStream 
]
