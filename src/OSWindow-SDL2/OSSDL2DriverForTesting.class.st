Class {
	#name : #OSSDL2DriverForTesting,
	#superclass : #OSSDL2Driver,
	#instVars : [
		'windowMap'
	],
	#category : #'OSWindow-SDL2-Tests'
}

{ #category : #private }
OSSDL2DriverForTesting >> critical: aBlock [
	^ aBlock value
]

{ #category : #private }
OSSDL2DriverForTesting >> windowMap [
	^ windowMap ifNil: [ windowMap := Dictionary new ]
]

{ #category : #private }
OSSDL2DriverForTesting >> windowMapMutex [
	^ self
]
