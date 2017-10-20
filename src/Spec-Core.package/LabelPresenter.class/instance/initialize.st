initialization
initialize
	super initialize.
	labelHolder := '' asValueHolder.
	emphasis := #().
	labelHolder whenChangedDo: [ self changed: #getText ].