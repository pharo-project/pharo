Class {
	#name : #Slice,
	#superclass : #KomitableObject,
	#category : #'Komitter-Models'
}

{ #category : #protocol }
Slice >> commitOn: aSubmitter [

	aSubmitter commitSlice: self
]
