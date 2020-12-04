Extension { #name : #SpTreePresenter }

{ #category : #'*Spec2-Commander2' }
SpTreePresenter >> contextMenuFromCommandsGroup: aValuable [
	self contextMenu: [ aValuable value beRoot asMenuPresenter ]
]
