Extension { #name : #Collection }

{ #category : #'*Zinc-Resource-Meta-Core' }
Collection >> addedToZnUrl: url [
	^ url withPathSegments: self
]
