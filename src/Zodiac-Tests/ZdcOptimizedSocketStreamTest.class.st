"
ZdcOptimizedSocketStreamTests tests ZdcOptimizedSocketStream
"
Class {
	#name : #ZdcOptimizedSocketStreamTest,
	#superclass : #ZdcAbstractSocketStreamTest,
	#category : #'Zodiac-Tests'
}

{ #category : #testing }
ZdcOptimizedSocketStreamTest class >> isAbstract [ 
	^ false
]

{ #category : #accessing }
ZdcOptimizedSocketStreamTest >> socketStreamClass [
	^ ZdcOptimizedSocketStream 
]
