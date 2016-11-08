protocol
updateShowIcon

	(self selected not and: [ self width < 100 ])
		ifTrue: [ self hideCloseIcon ]
		ifFalse: [ self showCloseIcon ]