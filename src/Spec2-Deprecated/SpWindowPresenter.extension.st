Extension { #name : #SpWindowPresenter }

{ #category : #'*Spec2-Deprecated' }
SpWindowPresenter >> hasToolBar [
	self deprecated: 'Use #hasToolbar instead. cf https://www.dictionary.com/browse/toolbar' transformWith: '`@receiver hasToolBar' -> '`@receiver hasToolbar'.
	^ self hasToolbar
]

{ #category : #'*Spec2-Deprecated' }
SpWindowPresenter >> modalRelativeTo: aWindow [
	self deprecated: 'Do not use this directly. Use #openModalWithSpec (and family) instead.' on: '2019-02-26' in: #Pharo8.

	self changed: #modalRelativeTo: with: {aWindow}
]

{ #category : #'*Spec2-Deprecated' }
SpWindowPresenter >> model [
	self deprecated: 'Model was renamed Presenter in Pharo 7' transformWith: '`@receiver model' -> '`@receiver presenter'.
	
	^ self presenter
]

{ #category : #'*Spec2-Deprecated' }
SpWindowPresenter >> model: aModel [
	self deprecated: 'Model was renamed Presenter in Pharo 7' transformWith: '`@receiver model: `@statements1' -> '`@receiver presenter: `@statements1'.

	self presenter: aModel
]
