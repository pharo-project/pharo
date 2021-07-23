"
Ring2 model of DoubleWordLayout
"
Class {
	#name : #RGDoubleWordLayout,
	#superclass : #RGBitsLayout,
	#category : #'Ring-Core-Kernel'
}

{ #category : #testing }
RGDoubleWordLayout >> isWordLayout [
	^ true
]

{ #category : #accessing }
RGDoubleWordLayout >> layoutName [

	^ #DoubleWordLayout
]
