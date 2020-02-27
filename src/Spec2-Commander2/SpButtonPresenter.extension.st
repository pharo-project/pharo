Extension { #name : #SpButtonPresenter }

{ #category : #'*Spec2-Commander2' }
SpButtonPresenter >> contextMenuFromCommandsGroup: aValuable [
	self contextMenu: [ aValuable value beRoot asMenuPresenter ]
]
