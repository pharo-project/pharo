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

{ #category : 'requirements' }
MetacelloImageRequest >> appliesReplaces [
	"Only the replaces of a root apply"

	^ false
]

{ #category : 'printing' }
MetacelloImageRequest >> chainDisplayNames [
	"The versions that led to a version the image requires: none, nobody asked before the image"

	^ #(  )
]

{ #category : 'printing' }
MetacelloImageRequest >> chainLine [
	"How the report says who asked, after 'Required by:': the image"

	^ self name
]

{ #category : 'printing' }
MetacelloImageRequest >> chainLines [
	"The versions that led to a version the image requires: none, the image has it"

	^ #(  )
]

{ #category : 'requests' }
MetacelloImageRequest >> chainNames [
	"The names of the projects that led to what the image asks for: none, nobody asked before the image"

	^ #(  )
]

{ #category : 'printing' }
MetacelloImageRequest >> displayName [
	"How the report names the image: the image"

	^ self name
]

{ #category : 'accessing' }
MetacelloImageRequest >> name [

	^ 'the image'
]

{ #category : 'printing' }
MetacelloImageRequest >> printOn: aStream [

	aStream nextPutAll: self name
]

{ #category : 'requirements' }
MetacelloImageRequest >> requiresInstalledVersions [

	^ true
]
