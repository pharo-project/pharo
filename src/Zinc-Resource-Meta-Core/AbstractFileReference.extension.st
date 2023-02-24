Extension { #name : #AbstractFileReference }

{ #category : #'*Zinc-Resource-Meta-Core' }
AbstractFileReference >> asUrl [
	"Convert the receiver in a file:// ZnUrl.
	Only an absolute path can be represented as a file:// URL"

	^ self asZnUrl
]

{ #category : #'*Zinc-Resource-Meta-Core' }
AbstractFileReference >> asZnUrl [
	"Convert the receiver in a file:// ZnUrl.
	Only an absolute path can be represented as a file:// URL"

	^ self asAbsolute path asZnUrl
]

{ #category : #'*Zinc-Resource-Meta-Core' }
AbstractFileReference >> mimeTypes [
	^ self resolve mimeTypes
]
