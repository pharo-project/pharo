Extension { #name : #TaRemoveMethod }

{ #category : #'*TraitsV2-Compatibility' }
TaRemoveMethod >> exclusions [
	^ removedSelectors
]

{ #category : #'*TraitsV2-Compatibility' }
TaRemoveMethod >> isTraitExclusion [
	^ true
]
