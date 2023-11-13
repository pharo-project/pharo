Extension { #name : 'TaRemoveMethod' }

{ #category : '*Traits-Compatibility' }
TaRemoveMethod >> exclusions [
	"For compatibility, does not use it!!!"
	^ removedSelectors
]

{ #category : '*Traits-Compatibility' }
TaRemoveMethod >> isTraitExclusion [
	^ true
]
