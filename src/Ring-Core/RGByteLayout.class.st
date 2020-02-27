Class {
	#name : #RGByteLayout,
	#superclass : #RGBitsLayout,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'testing types' }
RGByteLayout >> isByteLayout [

	^ true
]

{ #category : #accessing }
RGByteLayout >> layoutName [

	^ #ByteLayout
]
