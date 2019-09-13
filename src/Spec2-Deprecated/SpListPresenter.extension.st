Extension { #name : #SpListPresenter }

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> allowToSelect [
	"Return whether the list items can be selected or not"
	
	self deprecated: 'Should not use. Use #selectionMode instead'.
	^ allowToSelect
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> allowToSelect: aBoolean [
	"Set if the list items can be selected or not"

	self deprecated: 'Should not use. Use #selectionMode: instead'.
	allowToSelect := aBoolean.
	aBoolean ifFalse: [ self resetSelection ]
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> autoDeselect [
	"Return whether the list items can be selected if it's already the selected item"
	"If true, clicking again on the selected item will unselect it"

	self deprecated: 'Should not use. This feature is not supported anymore'.	
	^ autoDeselect
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> autoDeselect: aBoolean [
	"Set if the list items can be selected again"

	self deprecated: 'Should not use. This feature is not supported anymore'.
	autoDeselect := aBoolean
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> backgroundColorBlock [
	"Return the block use to compute the background color for each item"

	self deprecated: 'Should not use. This feature is not supported anymore. This feature depends on the theme'
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> backgroundColorBlock: aBlock [
	"Set the block used to compute an item background"

	self deprecated: 'Should not use. This feature is not supported anymore. This feature depends on the theme'
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> backgroundColorFor: anItem at: index [

	self deprecated: 'Should not use. This feature is not supported anymore. This feature depends on the theme'.
	^ self backgroundColorBlock cull: anItem cull: index
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> resetFilteringBlock [
	self
		deprecated: 'This feature is now removed from Spec 2. If the visible elements of the list need to be changed, it''s the users of the lists that should manage it and update the list of items of the list.'
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> whenAllowToSelectChanged: aBlock [
	self deprecated: 'Use #whenAllowToSelectChangedDo: instead.' transformWith: '`@receiver whenAllowToSelectChanged: `@statements' -> '`@receiver whenAllowToSelectChangedDo: `@statements'.
	self whenAllowToSelectChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> whenAllowToSelectChangedDo: aBlock [
	"Set a block to value when the value of allowToSelect has changed"
	self deprecated: 'Should not use. Use #selectionMode instead'.
	self property: #allowToSelect whenChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> whenAutoDeselectChanged: aBlock [
	self deprecated: 'Use #whenAutoDeselectChangedDo: instead.' transformWith: '`@receiver whenAutoDeselectChanged: `@statements' -> '`@receiver whenAutoDeselectChangedDo: `@statements'.
	self whenAutoDeselectChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> whenAutoDeselectChangedDo: aBlock [
	"Set a block to value when the value of autoDeselect has changed"

	self deprecated: 'Should not use. This feature is not supported anymore'.
	self property: #autoDeselect whenChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> whenBackgroundColorBlockChanged: aBlock [

	self deprecated: 'Should not use. This feature is not supported anymore. This feature depends on the theme'.

]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> whenDisplayBlockChanged: aBlock [
	self deprecated: 'Use #whenDisplayBlockChangedDo: instead.' transformWith: '`@receiver whenDisplayBlockChanged: `@statements' -> '`@receiver whenDisplayBlockChangedDo: `@statements'.
	self whenDisplayBlockChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> whenFilteringBlockChanged: aBlock [
	self deprecated: 'Use #whenFilteringBlockChangedDo: instead.' transformWith: '`@receiver whenFilteringBlockChanged: `@statements' -> '`@receiver whenFilteringBlockChangedDo: `@statements'.
	self whenFilteringBlockChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> whenFilteringBlockChangedDo: aBlock [
	"Set a block to value when the filtering block block has changed"

	self
		deprecated: 'This feature is now removed from Spec 2. If the visible elements of the list need to be changed, it''s the users of the lists that should manage it and update the list of items of the list.'
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> whenIconsChanged: aBlock [
	self deprecated: 'Use #whenIconsChangedDo: instead' transformWith: '`@receiver whenIconsChanged: `@argument' -> '`@receiver whenIconsChangedDo: `@argument'.
	self whenIconsChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> whenMenuChanged: aBlock [
	self deprecated: 'Use #whenMenuChangedDo: instead.' transformWith: '`@receiver whenMenuChanged: `@statements' -> '`@receiver whenMenuChangedDo: `@statements'.
	self whenMenuChangedDo: aBlock
]

{ #category : #'*Spec2-Deprecated' }
SpListPresenter >> whenSortingBlockChanged: aBlock [
	self deprecated: 'Use #whenSortingBlockChangedDo: instead.' transformWith: '`@receiver whenSortingBlockChanged: `@statements' -> '`@receiver whenSortingBlockChangedDo: `@statements'.
	self whenSortingBlockChangedDo: aBlock
]
