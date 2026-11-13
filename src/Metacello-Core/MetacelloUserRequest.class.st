"
I stand for whoever ran the script, as a requester: I ask for the roots of a load (see requestFor:).
"
Class {
	#name : 'MetacelloUserRequest',
	#superclass : 'Object',
	#category : 'Metacello-Core-Resolution',
	#package : 'Metacello-Core',
	#tag : 'Resolution'
}

{ #category : 'requests' }
MetacelloUserRequest >> chainNames [
	"The names of the projects that led to what I ask for: none, I asked first"

	^ OrderedCollection new
]

{ #category : 'accessing' }
MetacelloUserRequest >> name [

	^ 'the script'
]

{ #category : 'printing' }
MetacelloUserRequest >> printOn: aStream [

	aStream nextPutAll: self name
]

{ #category : 'requests' }
MetacelloUserRequest >> requestFor: aProjectSpec [
	"My request for the root aProjectSpec names"

	^ MetacelloProjectRequest requester: self projectSpec: aProjectSpec
]
