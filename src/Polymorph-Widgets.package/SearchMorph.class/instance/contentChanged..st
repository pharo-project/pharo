protocol
contentChanged: aString
	content := aString.
	self updateSelector ifNotNil: [ :selector|
		self model perform: selector with: aString ]