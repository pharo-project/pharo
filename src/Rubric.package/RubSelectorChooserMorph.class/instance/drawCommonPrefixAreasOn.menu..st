drawing
drawCommonPrefixAreasOn: aCanvas menu: aMenu
	| firstMenuItem topLeft bottomLeft lastMenuItem w c |
	firstMenuItem := aMenu menuItems first.
	lastMenuItem := aMenu menuItems last.
	w := (firstMenuItem fontToUse widthOfString: prefix).
	topLeft := firstMenuItem boundsInWorld topLeft.
	bottomLeft := lastMenuItem boundsInWorld bottomLeft + ( w @ 0 ).
	c := aMenu color muchDarker.
	aCanvas fillRectangle: (topLeft  corner: bottomLeft) color: (c alpha: 0.3)