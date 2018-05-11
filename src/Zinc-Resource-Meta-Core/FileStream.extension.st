Extension { #name : #FileStream }

{ #category : #'*zinc-resource-meta-core' }
FileStream >> asUrl [
	^ self asZnUrl
]

{ #category : #'*zinc-resource-meta-core' }
FileStream >> asZnUrl [
	"Convert the receiver in a file:// ZnUrl"

	| fileUrl |
	fileUrl := ZnUrl new.
	fileUrl scheme: #file.
	self directory pathSegments do: [ :each | fileUrl addPathSegment: each ].
	fileUrl addPathSegment: self localName.
	^ fileUrl
]

{ #category : #'*zinc-resource-meta-core' }
FileStream >> url [
	"Convert my path into a file:// type url String."
	
	^self asUrl asString
]
