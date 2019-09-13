Extension { #name : #SpAbstractListPresenter }

{ #category : #'*Spec2-Commander2' }
SpAbstractListPresenter >> contextMenuFromCommandsGroup: aValuable [
	self contextMenu: [ aValuable value beRoot asMenuPresenter ]
]
