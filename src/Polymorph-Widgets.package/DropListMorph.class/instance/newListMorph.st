drawing
newListMorph
	"Answer a new list morph"

	|m|
	m := (self listMorphClass
		on: self
		list: #list
		selected: #listSelectionIndex
		changeSelected: #listSelectionIndex:
		menu: nil
		keystroke: nil)
			autoDeselect: false;
			wrapSelector: #wrapItem:index:;
			roundedCorners: #(2 3);
			setProperty: #morphicLayerNumber toValue: 5;
			"color: self color;"
			borderStyle: (self theme dropListNormalListBorderStyleFor: self);
			on: #mouseDown send: #listMouseDown: to: self.
	^m