display
chooseMultipleFrom: anArray title: aString
	| window |
	window :=  TickDialogWindow 
					itemsList: anArray 
					itemsHeaderName: ''
					wrapBlockOrSelector: [:e | e ]
					title: aString
					defaultValue: true.
	^ anArray isEmpty
		ifTrue: [ anArray copyEmpty ]
		ifFalse: [ window chooseFromOwner: self model window ]