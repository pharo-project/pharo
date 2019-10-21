Extension { #name : #ClassDescription }

{ #category : #'*renraku' }
ClassDescription >> ban: aCritique [

	ReSystemAnnouncer uniqueInstance
		notifyCritique: aCritique
		of: aCritique sourceAnchor entity
		bannedOn: self.

	(TheManifestBuilder of: self)
		banRule: aCritique rule for: self.
]

{ #category : #'*renraku' }
ClassDescription >> banChecksForValidation [
"returns ban levels that have to be chacked before validation"

	^ { self }
]

{ #category : #'*renraku' }
ClassDescription >> banLevelCandidates [
"returns code entities which can ban a rule for this entity"

	^ { self } , self package banLevelCandidates
]

{ #category : #'*renraku' }
ClassDescription >> critiques [
	<eProperty>
	^ ReCriticEngine critiquesOf: self
]

{ #category : #'*renraku' }
ClassDescription >> reBanName [

	^ self name, ' (Class)'
]

{ #category : #'*renraku' }
ClassDescription >> suitableRulesFrom: aReRuleManager [ 
	^ aReRuleManager classRules
]
