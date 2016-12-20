scripting
alert: anObject
	|window|
	self alertOn: anObject.
	window := (self openInBrickWindowLabeled: 'Alert').
	self class environment at: #GLMSystemWindowBrick ifPresent: [ 
		window
			noMenu;
			innerExtent: self extent ].
