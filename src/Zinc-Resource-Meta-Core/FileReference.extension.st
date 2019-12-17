Extension { #name : #FileReference }

{ #category : #'*Zinc-Resource-Meta-Core' }
FileReference >> mimeTypes [
	"Return the possible mime types for the given path."
	^ filesystem mimeTypesAt: path
]
