dialog
openDialogWindowIn: aWindow title: aTitle selectedtems: aCollection 
	| dialog |
	dialog := self dialogWindowIn:  aWindow title: aTitle selectedtems: aCollection.
	aWindow openModal: dialog.
	^ dialog cancelled ifFalse: [self selectedItems]
