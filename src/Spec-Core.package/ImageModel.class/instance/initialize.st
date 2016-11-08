initialization
initialize
	super initialize.
	imageHolder := nil asValueHolder.
	actionHolder := [] asValueHolder. 
	autoScaleHolder := false asValueHolder.
	imageHolder whenChangedDo: [ self changed: #getImage ].