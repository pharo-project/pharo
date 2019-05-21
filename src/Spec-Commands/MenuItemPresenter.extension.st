Extension { #name : #MenuItemPresenter }

{ #category : #'*Spec-Commands' }
MenuItemPresenter >> keyText: aString [

	self flag: #TODO. "This is for backward compatibility and needs to be removed!"
	self shortcut: aString first meta
]
