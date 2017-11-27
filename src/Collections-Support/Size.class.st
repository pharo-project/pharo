Class {
	#name : #Size,
	#superclass : #Magnitude,
	#category : #'Collections-Support'
}

{ #category : #private }
Size class >> large [
	^ Large new
]

{ #category : #private }
Size class >> small [
	^ Small new
]
