initialization
initialize
	super initialize.
	
	self add: #(beForSmalltalkScripting beForSmalltalkCode) requiresPreSend: #model: