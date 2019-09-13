Extension { #name : #SpAbstractTextPresenter }

{ #category : #'*Spec2-Commander2' }
SpAbstractTextPresenter >> contextMenuFromCommandsGroup: aValuable [
	self contextMenu: [ aValuable value beRoot asMenuPresenter ]
]
