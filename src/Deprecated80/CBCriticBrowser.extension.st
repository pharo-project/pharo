Extension { #name : #CBCriticBrowser }

{ #category : #'*Deprecated80' }
CBCriticBrowser >> rule [
	self deprecated: 'Use #selectedRule instead.' transformWith: '`@receiver rule' -> '`@receiver selectedRule'.
	^ self selectedRule
]
