Class {
	#name : #RGWordLayout,
	#superclass : #RGBitsLayout,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'testing types' }
RGWordLayout >> isWordLayout [

	^ true
]

{ #category : #accessing }
RGWordLayout >> layoutName [

	^ #WordLayout
]
