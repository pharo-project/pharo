t - accessing
menu: menu shifted: b 
	self selectedNode ifNil: [ ^menu ].
	^ self selectedNode menu: menu shifted: b 