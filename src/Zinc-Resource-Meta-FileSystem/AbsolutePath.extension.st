Extension { #name : #AbsolutePath }

{ #category : #'*zinc-resource-meta-filesystem' }
AbsolutePath >> asUrl [ 
	^ self asZnUrl
]

{ #category : #'*zinc-resource-meta-filesystem' }
AbsolutePath >> asZnUrl [
	"Convert the receiver in a file:// ZnUrl"

	| fileUrl |
	fileUrl := ZnUrl new.
	fileUrl scheme: #file.
	self do: [ :each | fileUrl addPathSegment: each ].
	^ fileUrl
]
