construction
withCloseBox

	closeBox ifNotNil: [ closeBox delete ].	
		
	closeBox := self 
		iconButtonCalling: #closeBoxClicked 
		withForm: self theme menuCloseForm 
		helpText: 'Close this menu'.
		
	self addMorph: closeBox asElementNumber: 1