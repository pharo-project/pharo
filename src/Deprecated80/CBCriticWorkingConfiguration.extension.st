Extension { #name : #CBCriticWorkingConfiguration }

{ #category : #'*Deprecated80' }
CBCriticWorkingConfiguration >> rule [
	self deprecated: 'Use #rules instead' transformWith: '`@receiver rule' -> '`@receiver rules'.
	^ self rules
]

{ #category : #'*Deprecated80' }
CBCriticWorkingConfiguration >> rule: anObject [
	self deprecated: 'Use #rules: instead' transformWith: '`@receiver rule: `@statements1' -> '`@receiver rules: `@statements1'.
	self rules: anObject
]
