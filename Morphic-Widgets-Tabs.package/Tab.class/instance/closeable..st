protocol
closeable: anObject

	closeable := anObject.
	anObject 
		ifTrue: [ self showCloseIcon ]
		ifFalse: [ self hideCloseIcon ]