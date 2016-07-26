accessing
selectedItemBrick: aBrick
	
	selectedItemBrick isNil & aBrick isNotNil ifTrue: [ self showTriangle ].
	selectedItemBrick isNotNil & aBrick isNil ifTrue: [ self hideTriangle ].
	
	selectedItemBrick := aBrick