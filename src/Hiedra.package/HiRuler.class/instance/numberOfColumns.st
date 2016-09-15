accessing
numberOfColumns
	^ freeColumnIndexByRow 
		ifEmpty: [ 0 ]
		ifNotEmpty: [ freeColumnIndexByRow max ]