removing
removeAGroup: aGroup

	| yesIWantToRemoveTheGroup |
	yesIWantToRemoveTheGroup := self openRemoveDialogOn: aGroup.
	yesIWantToRemoveTheGroup
		ifFalse: [ ^ nil ].
	^ self removeAGroupSilently: aGroup
