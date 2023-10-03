Extension { #name : 'RPackage' }

{ #category : '*Renraku' }
RPackage >> ban: aCritique [

	ReSystemAnnouncer uniqueInstance
		notifyCritique: aCritique
		of: aCritique sourceAnchor entity
		bannedOn: self.

	(TheManifestBuilder of: self)
		banRule: aCritique rule for: self
]

{ #category : '*Renraku' }
RPackage >> banChecksForValidation [

	^ {}
]

{ #category : '*Renraku' }
RPackage >> banLevelCandidates [
"returns code entities which can ban a rule for this entity"

	^ { self . ReImageRuleBaner . RePersistentImageRuleBaner }
]

{ #category : '*Renraku' }
RPackage >> critiques [
	^ ReCriticEngine critiquesOf: self
]

{ #category : '*Renraku' }
RPackage >> package [
	^ self
]

{ #category : '*Renraku' }
RPackage >> reBanName [

	^ self name, ' (Package)'
]

{ #category : '*Renraku' }
RPackage >> suitableRulesFrom: aReRuleManager [
	^ aReRuleManager packageRules
]
