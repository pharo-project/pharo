Class {
	#name : #RGEmptyLayout,
	#superclass : #RGLayout,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'testing types' }
RGEmptyLayout >> isEmptyLayout [

	^ true
]

{ #category : #accessing }
RGEmptyLayout >> layoutName [

	^ #EmptyLayout
]
