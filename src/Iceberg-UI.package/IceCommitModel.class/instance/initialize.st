initialization
initialize
	model := nil asValueHolder.
	model whenChangedDo: [ :diff | commitButton label: 'Commit onto ', diff repository branch name ].
	model whenChangedDo: [ :diff | commitPushButton label: 'Commit and Push onto ', diff repository branch name ].
	super initialize