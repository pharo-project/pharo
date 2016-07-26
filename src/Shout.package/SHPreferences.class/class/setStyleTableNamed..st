settings
setStyleTableNamed: aString
	self setStyleTable: (((Pragma allNamed: #styleTable: in: SHTextStylerST80 class)
		detect: [ :each | (each argumentAt: 1) = aString ])
		method 
			valueWithReceiver: SHTextStylerST80 class 
			arguments: #())