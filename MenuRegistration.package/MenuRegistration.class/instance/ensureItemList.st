accessing
ensureItemList
	"Return the list of MenuRegistration instances"
	^ itemList ifNil: [itemList := OrderedCollection new]