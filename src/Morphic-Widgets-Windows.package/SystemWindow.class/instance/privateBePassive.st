activation
privateBePassive
	"private"
	self isInWorld ifFalse: [ ^self ].
	
	self 
	rememberKeyboardFocus: ActiveHand keyboardFocus;
	passivate;
	announceDeActivated

	
	
	