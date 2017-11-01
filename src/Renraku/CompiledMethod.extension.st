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
	| overridenMethods |
	
	overridenMethods := OrderedCollection new.

	self methodClass allSuperclasses
		do: [ :class |
			class compiledMethodAt: self selector
				ifPresent: [ :method | overridenMethods add: method ]
				ifAbsent: [ ] ].
			
	^ overridenMethods 
]

{ #category : #'*renraku' }
CompiledMethod >> reBanName [

	^ self name, ' (Method)'
]

{ #category : #'*renraku' }
CompiledMethod >> suitableRulesFrom: aReRuleManager [ 
	^ aReRuleManager methodRules
]
