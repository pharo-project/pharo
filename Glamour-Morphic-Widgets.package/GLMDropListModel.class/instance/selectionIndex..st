accessing
selectionIndex: anObject
	selectionIndex := anObject.
	self announcer announce: (
			GLMDropDownListMorphSelectionChanged new 
				selectionValue: self selectedItem;
				selectionIndex: selectionIndex;
				yourself)