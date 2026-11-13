"
I stand for the image, as a requester: I ask for the versions it has installed, so that they are never downgraded.
"
Class {
	#name : 'MetacelloImageRequest',
	#superclass : 'Object',
	#category : 'Metacello-Core-Resolution',
	#package : 'Metacello-Core',
	#tag : 'Resolution'
}

{ #category : 'requests' }
MetacelloImageRequest >> chainNames [
	"The names of the projects that led to what the image asks for: none, nobody asked before the image"

	^ #(  )
]

{ #category : 'accessing' }
MetacelloImageRequest >> name [

	^ 'the image'
]

{ #category : 'printing' }
MetacelloImageRequest >> printOn: aStream [

	aStream nextPutAll: self name
]
