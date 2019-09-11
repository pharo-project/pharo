Extension { #name : #DropListPresenter }

{ #category : #'*Spec-Deprecated80' }
DropListPresenter >> whenSelectedItemChanged: aBlock [
	self deprecated: 'Use #whenSelectedItemChangedDo: instead.' transformWith: '`@receiver whenSelectedItemChanged: `@statements' -> '`@receiver whenSelectedItemChangedDo: `@statements'.
	self whenSelectedItemChangedDo: aBlock
]

{ #category : #'*Spec-Deprecated80' }
DropListPresenter >> whenSelectionChanged: aBlock [
	self deprecated: 'Use #whenSelectionChangedDo: instead.' transformWith: '`@receiver whenSelectionChanged: `@statements' -> '`@receiver whenSelectionChangedDo: `@statements'.
	self whenSelectionChangedDo: aBlock
]

{ #category : #'*Spec-Deprecated80' }
DropListPresenter >> whenSelectionIndexChanged: aBlock [
	self deprecated: 'Use #whenSelectionIndexChangedDo: instead.' transformWith: '`@receiver whenSelectionIndexChanged: `@statements' -> '`@receiver whenSelectionIndexChangedDo: `@statements'.
	self whenSelectionIndexChangedDo: aBlock
]
