Extension { #name : #FTFunction }

{ #category : #'*Deprecated80' }
FTFunction >> beExplicite [
	self deprecated: 'Use #beExplicit instead' transformWith: '`@receiver beExplicite' -> '`@receiver beExplicit'.
	^ self beExplicit
]

{ #category : #'*Deprecated80' }
FTFunction >> isExplicite [
	self deprecated: 'Use #isExplicit instead' transformWith: '`@receiver isExplicite' -> '`@receiver isExplicit'.
	^ self isExplicit
]
