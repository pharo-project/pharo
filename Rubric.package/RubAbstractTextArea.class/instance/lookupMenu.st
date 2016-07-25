menu
lookupMenu
	"default implementation of the algorithm that lookup the menu"

	^ self model 
		ifNil: [ self editingMode menu ] 
		ifNotNil: [ :m | m menu ifNil: [ self editingMode menu ] ]