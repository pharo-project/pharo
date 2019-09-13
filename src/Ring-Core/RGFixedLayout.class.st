Class {
	#name : #RGFixedLayout,
	#superclass : #RGPointerLayout,
	#category : #'Ring-Core-Kernel'
}

{ #category : #'testing types' }
RGFixedLayout >> isFixedLayout [

	^ true
]

{ #category : #accessing }
RGFixedLayout >> layoutName [

	^ #FixedLayout
]
