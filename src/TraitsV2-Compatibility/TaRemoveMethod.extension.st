Extension { #name : #TaRemoveMethod }

{ #category : #'*TraitsV2-Compatibility' }
TaRemoveMethod >> exclusions [
	"For compatibility, does not use it!!!"
	^ removedSelectors
]

{ #category : #'*TraitsV2-Compatibility' }
TaRemoveMethod >> isTraitExclusion [
	^ true
]
