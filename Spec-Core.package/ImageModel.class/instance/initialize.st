initialization
initialize
	super initialize.
	imageHolder := nil asValueHolder.
	actionHolder := [] asValueHolder. 
	imageHolder whenChangedDo: [ self changed: #getImage ].