configure
configureSystemWindow: aWindow 
	aWindow basicBorderColor: Color lightGray.
	aWindow basicBorderWidth: 1.
	aWindow color: Color lightGray lighter lighter lighter.	
	self
		configureWindowBorderFor: aWindow;
		configureWindowDropShadowFor: aWindow.	
