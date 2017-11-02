Extension { #name : #RPackage }

{ #category : #'*renraku' }
RPackage >> ban: aCritique [

	ReSystemAnnouncer uniqueInstance
		notifyCritique: aCritique
		of: aCritique sourceAnchor entity
		bannedOn: self.

	(TheManifestBuilder of: self)
		banRule: aCritique rule for: self.
]

{ #category : #'*Renraku' }
RPackage >> banChecksForValidation [
"returns ban levels that have to be chacked before validation"

	^ #()
]

{ #category : #'*Renraku' }
RPackage >> banLevelCandidates [
"returns code entities which can ban a rule for this entity"

	^ { self . ReImageRuleBaner . RePersistentImageRuleBaner }  
]

{ #category : #'*renraku' }
RPackage >> critiques [
	<eProperty>
	
	^ ReCriticEngine critiquesOf: self
]

{ #category : #'*renraku' }
RPackage >> package [
	^ self
]

{ #category : #'*renraku' }
RPackage >> reBanName [

	^ self name, ' (Package)'
]

{ #category : #'*renraku' }
RPackage >> suitableRulesFrom: aReRuleManager [ 
	^ aReRuleManager packageRules
]
