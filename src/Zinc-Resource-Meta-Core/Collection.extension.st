Extension { #name : #Collection }

{ #category : #'*zinc-resource-meta-core' }
Collection >> addedToZnUrl: url [ 
	^ url withPathSegments: self
]
