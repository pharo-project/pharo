selection
projectBoundToSelection
	| selection |
	
	selection := projectList selectedItem.
	selection ifNil: [ ^ nil ].
	^ (selection parentNode)
		ifNil: [ selection content ]
		ifNotNil: [ selection parentNode content ]