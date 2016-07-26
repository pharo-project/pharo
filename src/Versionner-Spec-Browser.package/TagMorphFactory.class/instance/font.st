accessing
font
	^ (LogicalFont 
		familyName: TextStyle defaultFont familyName 
		pointSize: 8)
		emphasis: 1.