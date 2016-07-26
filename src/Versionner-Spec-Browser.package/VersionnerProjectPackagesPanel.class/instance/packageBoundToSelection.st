selection
packageBoundToSelection
	| selection |
	
	selection := packagesList selectedItem.
	selection ifNil: [ ^ nil ].
	^ (selection parentNode)
		ifNil: [ selection content ]
		ifNotNil: [ selection parentNode content ]