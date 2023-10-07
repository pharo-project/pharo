Extension { #name : 'AbsolutePath' }

{ #category : '*Zinc-Resource-Meta-Core' }
AbsolutePath >> asUrl [
	"Convert the receiver in a file:// ZnUrl"

	^ self asZnUrl
]

{ #category : '*Zinc-Resource-Meta-Core' }
AbsolutePath >> asZnUrl [
	"Convert the receiver in a file:// ZnUrl"

	| fileUrl |
	fileUrl := ZnUrl new.
	fileUrl scheme: #file.
	self do: [ :each | fileUrl addPathSegment: each ].
	^ fileUrl
]
