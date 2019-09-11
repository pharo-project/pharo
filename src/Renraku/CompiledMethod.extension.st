Extension { #name : #CompiledMethod }

{ #category : #'*renraku' }
CompiledMethod >> ban: aCritique [

	ReSystemAnnouncer uniqueInstance
		notifyCritique: aCritique
		of: aCritique sourceAnchor entity
		bannedOn: self.

	(TheManifestBuilder of: self)
		banRule: aCritique rule for: self.
]

{ #category : #'*renraku' }
CompiledMethod >> banChecksForValidation [
"returns ban levels that have to be checked before validation"

	^ { self . self methodClass }

]

{ #category : #'*renraku' }
CompiledMethod >> banLevelCandidates [
"returns code entities which can ban a rule for this entity"

	^ { self } , self methodClass banLevelCandidates
]

{ #category : #'*renraku' }
CompiledMethod >> critiques [
	<eProperty>
	| critiques |
	
	critiques := ReCriticEngine critiquesOf: self.
	critiques :=
		critiques,
		(ReCriticEngine nodeCritiquesOf: self).
	
	^ critiques
]

{ #category : #'*renraku' }
CompiledMethod >> glmCritiqueSourceIn: composite [

	^ self gtInspectorSourceIn: composite
	
]

{ #category : #'*renraku' }
CompiledMethod >> overriddenMethods [
	"all the methods of my superclasses that are overriden by me"
	| selector |
	selector := self selector.
	^ self methodClass allSuperclasses
		select: [ :class | class includesSelector: selector ]
		thenCollect: [ :class | class compiledMethodAt: selector ]
]

{ #category : #'*renraku' }
CompiledMethod >> reBanName [

	^ self name, ' (Method)'
]

{ #category : #'*renraku' }
CompiledMethod >> suitableRulesFrom: aReRuleManager [ 
	^ aReRuleManager methodRules
]
