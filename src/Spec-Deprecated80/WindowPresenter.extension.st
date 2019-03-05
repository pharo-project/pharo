Extension { #name : #WindowPresenter }

{ #category : #'*Spec-Deprecated80' }
WindowPresenter >> model [
	self deprecated: 'Model was renamed Presenter in Pharo 7' transformWith: '`@receiver model' -> '`@receiver presenter'.
	
	^ self presenter
]

{ #category : #'*Spec-Deprecated80' }
WindowPresenter >> model: aModel [
	self deprecated: 'Model was renamed Presenter in Pharo 7' transformWith: '`@receiver model: `@statements1' -> '`@receiver presenter: `@statements1'.

	self presenter: aModel
]
