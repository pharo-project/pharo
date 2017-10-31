accessing
list
	^ list ifNil: [ 
		list := self instantiate: FastTablePresenter.
		list 
			handlesDoubleClick: true;
			whenSelectedItemChanged: [ :item |
				self refreshDescription: item ];
			doubleClickAction: [ self diveIntoSelectedObject ]. ]