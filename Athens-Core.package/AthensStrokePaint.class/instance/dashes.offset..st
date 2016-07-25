setting dashes
dashes: anAlternateCollectionOfLenghts offset: anOffset
	
	'self dashes: #( "fill" 20  "gap" 10 "fill" 35 "gap" 30) offset: 0. '.
	
	self subclassResponsibility

	