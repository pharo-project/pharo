accessing
setContainer: anObject

	viewContainer := anObject.
	
	viewContainer layoutView announcer weak when: TxLayoutViewChanged send: #viewChanged: to: self