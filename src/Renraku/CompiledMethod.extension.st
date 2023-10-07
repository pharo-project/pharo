Extension { #name : 'CompiledMethod' }

{ #category : '*Renraku' }
CompiledMethod >> ban: aCritique [

	ReSystemAnnouncer uniqueInstance
		notifyCritique: aCritique
		of: aCritique sourceAnchor entity
		bannedOn: self.

	(TheManifestBuilder of: self)
		banRule: aCritique rule for: self
]

{ #category : '*Renraku' }
CompiledMethod >> banChecksForValidation [
"returns ban levels that have to be checked before validation"

	^ { self . self methodClass }
]

{ #category : '*Renraku' }
CompiledMethod >> banLevelCandidates [
"returns code entities which can ban a rule for this entity"

	^ { self } , self methodClass banLevelCandidates
]

{ #category : '*Renraku' }
CompiledMethod >> critiques [
	| critiques |

	critiques := ReCriticEngine critiquesOf: self.
	critiques :=
		critiques,
		(ReCriticEngine nodeCritiquesOf: self).

	^ critiques
]

{ #category : '*Renraku' }
CompiledMethod >> overriddenMethods [
	"all the methods of my superclasses that are overriden by me"
	| selector |
	selector := self selector.
	^ self methodClass allSuperclasses
		select: [ :class | class includesSelector: selector ]
		thenCollect: [ :class | class compiledMethodAt: selector ]
]

{ #category : '*Renraku' }
CompiledMethod >> reBanName [

	^ self name, ' (Method)'
]

{ #category : '*Renraku' }
CompiledMethod >> suitableRulesFrom: aReRuleManager [
	^ aReRuleManager methodRules
]
