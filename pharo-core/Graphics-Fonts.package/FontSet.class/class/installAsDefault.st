installing
installAsDefault 
	"FontSetNewYork installAsDefault"
	
	(self confirm: 'Do you want to install' translated, '
''' , self fontName , ''' as default font?' translated)
		ifFalse: [^ self].
	self installAsTextStyle.
	TextSharedInformation at: #DefaultTextStyle put: (TextStyle named: self fontName).
	
