menu building
newSubItem
	| reg |
	reg := MenuRegistration owner: currentRoot.
	currentRoot addItem: reg.
	^ reg