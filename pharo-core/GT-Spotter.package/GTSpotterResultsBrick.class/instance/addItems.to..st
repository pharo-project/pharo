adding
addItems: aCollectionOfItems to: aCategoryBrick
	"Add new items from a collection to the specified category. If category brick has no items
	it will be added to spotter ui.
	aCollectionOfItems must not be nil.
	aCollectionOfItems must not be empty.
	aCategoryBrick must not be nil"
	|shouldAddCategory|
	self assert: aCollectionOfItems isNotNil.
	self assert: aCollectionOfItems isNotEmpty.
	self assert: aCategoryBrick isNotNil.
	
	"if category has no items we need to add it in the list"
	shouldAddCategory := aCategoryBrick hasSubbricks not.
	"add new item"
	aCategoryBrick addItems: aCollectionOfItems.
	"add category to the list"
	shouldAddCategory ifTrue: [ self addCategory: aCategoryBrick ]
	
	