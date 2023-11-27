Extension { #name : 'Package' }

{ #category : '*Renraku' }
Package >> ban: aCritique [

	ReSystemAnnouncer uniqueInstance
		notifyCritique: aCritique
		of: aCritique sourceAnchor entity
		bannedOn: self.

	(TheManifestBuilder of: self)
		banRule: aCritique rule for: self
]

{ #category : '*Renraku' }
Package >> banChecksForValidation [

	^ {}
]

{ #category : '*Renraku' }
Package >> banLevelCandidates [
"returns code entities which can ban a rule for this entity"

	^ { self . ReImageRuleBaner . RePersistentImageRuleBaner }
]

{ #category : '*Renraku' }
Package >> critiques [
	^ ReCriticEngine critiquesOf: self
]

{ #category : '*Renraku' }
Package >> package [
	^ self
]

{ #category : '*Renraku' }
Package >> reBanName [

	^ self name, ' (Package)'
]

{ #category : '*Renraku' }
Package >> suitableRulesFrom: aReRuleManager [
	^ aReRuleManager packageRules
]
