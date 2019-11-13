Extension { #name : #SpTreeTablePresenter }

{ #category : #'*Spec2-Commander2' }
SpTreeTablePresenter >> contextMenuFromCommandsGroup: aValuable [
	self contextMenu: [ aValuable value beRoot asMenuPresenter ]
]
