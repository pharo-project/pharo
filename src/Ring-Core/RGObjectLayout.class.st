Class {
	#name : 'RGObjectLayout',
	#superclass : 'RGLayout',
	#category : 'Ring-Core-Kernel',
	#package : 'Ring-Core',
	#tag : 'Kernel'
}

{ #category : 'testing - types' }
RGObjectLayout >> isObjectLayout [

	^ true
]

{ #category : 'accessing' }
RGObjectLayout >> slots [

	"only for API compatibility purposes"
	^ Array new
]
