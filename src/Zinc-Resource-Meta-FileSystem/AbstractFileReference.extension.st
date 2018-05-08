Extension { #name : #AbstractFileReference }

{ #category : #'*zinc-resource-meta-filesystem' }
AbstractFileReference >> asUrl [
	"Convert the receiver in a file:// ZnUrl.
	Only an absolute path can be represented as a file:// URL"
	
	^ self asZnUrl
]

{ #category : #'*zinc-resource-meta-filesystem' }
AbstractFileReference >> asZnUrl [
	"Convert the receiver in a file:// ZnUrl.
	Only an absolute path can be represented as a file:// URL"
	
	^ self asAbsolute path asZnUrl
]
