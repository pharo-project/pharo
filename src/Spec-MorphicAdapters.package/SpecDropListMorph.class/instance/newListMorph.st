drawing
newListMorph
	"Answer a new list morph"

	^ (self listMorphClass
		on: self
		list: #list
		selected: #listSelectionIndex
		changeSelected: #listSelectionIndex:
		menu: nil
		keystroke: nil)
			getIconSelector: #getIconFor:;
			autoDeselect: false;
			wrapSelector: #wrapItem:index:;
			roundedCorners: #(2 3);
			setProperty: #morphicLayerNumber toValue: 5;
			borderStyle: (self theme dropListNormalListBorderStyleFor: self);
			on: #mouseDown send: #listMouseDown: to: self;
			yourself