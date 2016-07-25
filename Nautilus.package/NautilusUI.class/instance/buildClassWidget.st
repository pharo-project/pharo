widget class
buildClassWidget
	| classesList |
	classesList := classWidget buildClassesList.
	self setShortcuts: #NautilusClassShortcuts to: classesList.
	^ classesList