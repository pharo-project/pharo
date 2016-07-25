menus
menu: aMenu shifted: aBoolean 
	self selectedPackage ifNil: [ ^ aMenu ].

	^ self selectedPackage 
		menu: aMenu 
		shifted: aBoolean
		target: self