scripting
alert: anObject
	|window|
	self alertOn: anObject.
	window := (self openInBrickWindowLabeled: 'Alert').
	#GLMSystemWindowBrick asClassIfAbsent: [ ^ self ].
	window
		noMenu;
		innerExtent: self extent