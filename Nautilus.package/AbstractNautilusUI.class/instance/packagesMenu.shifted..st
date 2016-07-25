menu builder
packagesMenu: aMenu shifted: aBoolean 
	^ aMenu addAllFrom: (self packageMenuBuilder menu)