Extension { #name : #TheManifestBuilder }

{ #category : #'*renraku' }
TheManifestBuilder >> banRule: aRule for: anEntity [
	| ruleId number |
	ruleId := aRule class uniqueIdentifierName.
	number := aRule class identifierMinorVersionNumber.

	(self hasFalsePositiveOf: ruleId version: number) ifFalse: [ 
		self installFalsePositiveOf: ruleId version: number ].
	
	self
		addFalsePositive: anEntity
		of: ruleId
		version: number
]

{ #category : #'*renraku' }
TheManifestBuilder >> bansRule: aRule for: anEntity [
	| ruleId number |
	ruleId := aRule class uniqueIdentifierName.
	number := aRule class identifierMinorVersionNumber.
	
	^ self
		containsFalsePositive: anEntity
		onRule: ruleId
		version: number
]
