Extension { #name : #String }

{ #category : #'*Zinc-Resource-Meta-Core' }
String >> addedToZnUrl: url [
	| segments |
	segments := self findTokens: '/'.
	^ url withPathSegments: segments
]

{ #category : #'*Zinc-Resource-Meta-Core' }
String >> asUrl [
	^ self asZnUrl
]

{ #category : #'*Zinc-Resource-Meta-Core' }
String >> asZnMimeType [
	^ ZnMimeType fromString: self
]

{ #category : #'*Zinc-Resource-Meta-Core' }
String >> asZnUrl [
	^ ZnUrl fromString: self
]
