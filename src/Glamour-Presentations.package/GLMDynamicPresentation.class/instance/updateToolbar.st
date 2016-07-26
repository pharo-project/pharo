updating
updateToolbar
	super updateToolbar.
	cachedPresentation ifNotNil: [ 
		cachedPresentation updateToolbar.
		cachedPresentation presentations do: #updateToolbar ]