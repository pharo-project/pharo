construction
withPinBox

	pinBox ifNotNil: [ pinBox delete ].	
		
	pinBox := self 
		iconButtonCalling: #pinBoxClicked 
		withForm: self pinForm
		helpText: 'Keep this menu up'.	
			
	self addMorph: pinBox asElementNumber: 4