access
color: aFillStyle
	color := aFillStyle.
	self tabs do: [ :anAssociation |
		anAssociation key color: aFillStyle ]
