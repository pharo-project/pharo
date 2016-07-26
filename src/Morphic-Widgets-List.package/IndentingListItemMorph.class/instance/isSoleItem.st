accessing
isSoleItem
	^self isFirstItem and: [ owner submorphs size = 1 ]