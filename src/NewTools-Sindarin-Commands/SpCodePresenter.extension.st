Extension { #name : #SpCodePresenter }

{ #category : #'*NewTools-Sindarin-Commands' }
SpCodePresenter >> nonEmptySelectionInterval [
	| selectionInterval |
	selectionInterval := self selectionInterval.
	selectionInterval isEmpty
		ifTrue: [ ^self selectLine ].
	^ selectionInterval
]
