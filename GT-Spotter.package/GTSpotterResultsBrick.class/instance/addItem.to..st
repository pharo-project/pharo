adding
addItem: anItemBrick to: aCategoryBrick
	"Add new item brick to the specified category. If category brick has no items
	it will be added to spotter ui.
	anItemBrick must not be nil.
	aCategoryBrick must not be nil"
	|shouldAddCategory|
	self assert: anItemBrick isNotNil.
	self assert: aCategoryBrick isNotNil.
	
	"if category has no items we need to add it in the list"
	shouldAddCategory := aCategoryBrick hasSubbricks not.
	"add new item"
	aCategoryBrick addItem: anItemBrick.
	"add category to the list"
	shouldAddCategory ifTrue: [ self addCategory: aCategoryBrick ]
	
	