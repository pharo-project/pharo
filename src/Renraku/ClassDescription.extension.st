Extension { #name : 'ClassDescription' }

{ #category : '*Renraku' }
ClassDescription >> ban: aCritique [

	ReSystemAnnouncer uniqueInstance
		notifyCritique: aCritique
		of: aCritique sourceAnchor entity
		bannedOn: self.

	(TheManifestBuilder of: self)
		banRule: aCritique rule for: self
]

{ #category : '*Renraku' }
ClassDescription >> banChecksForValidation [
"returns ban levels that have to be chacked before validation"

	^ { self }
]

{ #category : '*Renraku' }
ClassDescription >> banLevelCandidates [
"returns code entities which can ban a rule for this entity"

	^ { self } , self package banLevelCandidates
]

{ #category : '*Renraku' }
ClassDescription >> critiques [
	^ ReCriticEngine critiquesOf: self
]

{ #category : '*Renraku' }
ClassDescription >> reBanName [

	^ self name, ' (Class)'
]

{ #category : '*Renraku' }
ClassDescription >> suitableRulesFrom: aReRuleManager [
	^ aReRuleManager classRules
]
