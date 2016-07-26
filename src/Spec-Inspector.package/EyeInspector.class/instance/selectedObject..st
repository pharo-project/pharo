accessing
selectedObject: anObject
	"Sets the selectedElement of the inspector to anObject. If anObject is not in the list, then sets the selectedElement to 1"

	self list setSelectedIndex: 1.
	self list listItems withIndexDo: [ :item :index | 
		item value == anObject 
			ifTrue: [ self list setSelectedIndex: index ] ]