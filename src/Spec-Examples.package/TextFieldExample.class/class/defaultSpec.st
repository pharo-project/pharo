specs
defaultSpec
	| textFieldHeight |
	
	textFieldHeight := StandardFonts defaultFont height + 15.
	^ SpecLayout composed
	 	add: #textField origin: 0@0 corner: 1@0 offsetOrigin: 0@0 offsetCorner: 0@textFieldHeight;
	 	add: #methodBrowser origin: 0@0 corner: 1@1 offsetOrigin: 0@textFieldHeight offsetCorner: 0@textFieldHeight;
		yourself 