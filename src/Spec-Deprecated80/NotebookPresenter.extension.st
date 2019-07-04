Extension { #name : #NotebookPresenter }

{ #category : #'*Spec-Deprecated80' }
NotebookPresenter >> whenSelectedPageChanged: aBlock [
	self deprecated: 'Use #whenSelectedPageChangedDo: instead.' transformWith: '`@receiver whenSelectedPageChanged: `@statements' -> '`@receiver whenSelectedPageChangedDo: `@statements'.
	self whenSelectedPageChangedDo: aBlock
]
