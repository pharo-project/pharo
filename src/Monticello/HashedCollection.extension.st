Extension { #name : #HashedCollection }

{ #category : #'*Monticello-Storing' }
HashedCollection >> comeFullyUpOnReload: smartRefStream [
	"Symbols have new hashes in this image."

	self  compact.
	"^ self"

]
