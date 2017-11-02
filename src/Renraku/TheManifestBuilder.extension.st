Extension { #name : #TheManifestBuilder }

{ #category : #'*renraku' }
TheManifestBuilder >> banRule: aRule for: anEntity [
	| name number |
	  name := aRule class name.
	number := aRule class identifierMinorVersionNumber.

	(self hasFalsePositiveOf: name version: number) ifFalse: [ 
		self installFalsePositiveOf: name version: number ].
	
	self
		addFalsePositive: anEntity
		of: name
		version: number
]

{ #category : #'*renraku' }
TheManifestBuilder >> bansRule: aRule for: anEntity [
	| name number |
	  name := aRule class name.
	number := aRule class identifierMinorVersionNumber.
	
	^ self
		containsFalsePositive: anEntity onRule: name version: number
]
