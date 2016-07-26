composing
scrollBounds
	^ scrollPane
		ifNil: [ self innerBounds  ]
		ifNotNil: [ :h | scrollPane scrollBounds ]