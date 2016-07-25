accessing
list
	^ list ifNil: [ 
		list := self instantiate: FastTableModel.
		list 
			handlesDoubleClick: true;
			whenSelectedItemChanged: [ :item |
				self refreshDescription: item ];
			doubleClickAction: [ self diveIntoSelectedObject ]. ]