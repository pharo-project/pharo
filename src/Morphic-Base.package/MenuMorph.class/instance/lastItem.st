accessing
lastItem

	submorphs reverseDo: [ :each | (each isMenuItemMorph) ifTrue: [ ^each ] ].
	^submorphs last