accessing
showInstance: aBoolean

	showInstance := aBoolean.
	self announcer announce: ( NautilusShowInstanceChanged boolean: aBoolean )