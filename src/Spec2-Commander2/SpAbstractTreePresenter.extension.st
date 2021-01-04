Extension { #name : #SpAbstractTreePresenter }

{ #category : #'*Spec2-Commander2' }
SpAbstractTreePresenter >> contextMenuFromCommandsGroup: aValuable [
	self contextMenu: [ aValuable value beRoot asMenuPresenter ]
]
