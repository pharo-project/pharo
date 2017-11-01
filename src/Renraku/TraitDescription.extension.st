Extension { #name : #TraitDescription }

{ #category : #'*renraku' }
TraitDescription >> ban: aCritique [

	ReSystemAnnouncer uniqueInstance
		notifyCritique: aCritique
		of: aCritique sourceAnchor entity
		bannedOn: self.

	(TheManifestBuilder of: self)
		banRule: aCritique rule for: self.
]

{ #category : #'*renraku' }
TraitDescription >> banChecksForValidation [
"returns ban levels that have to be chacked before validation"

	^ { self }
]

{ #category : #'*renraku' }
TraitDescription >> banLevelCandidates [
"returns code entities which can ban a rule for this entity"

	^ { self } , self package banLevelCandidates
]

{ #category : #'*renraku' }
TraitDescription >> critiques [
	<eProperty>
	
	^ ReCriticEngine critiquesOf: self
]

{ #category : #'*renraku' }
TraitDescription >> glmCritiqueSourceIn: composite [

	^ composite pharoScript  
		title: 'Source';
		display: [ self definition ];
		act: [ self browse ] icon: GLMUIThemeExtraIcons glamorousBrowse entitled: 'Browse'
]

{ #category : #'*renraku' }
TraitDescription >> reBanName [

	^ self name, ' (Trait)'
]

{ #category : #'*renraku' }
TraitDescription >> suitableRulesFrom: aReRuleManager [ 
	^ aReRuleManager classRules 
]
