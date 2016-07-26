accessing
hasSubMenu: aMenuMorph
	subMenu ifNil:[^false].
	subMenu == aMenuMorph ifTrue:[^true].
	^subMenu hasSubMenu: aMenuMorph