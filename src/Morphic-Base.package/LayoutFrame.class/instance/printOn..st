printing
printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: ' ( '.
	aStream
		print: self leftFraction @ self topFraction; 
		nextPutAll: ' + ';
		print: self leftOffset  @ self topOffset ;
		nextPutAll: 'px corner: ';
		print: self rightFraction @ self bottomFraction; 
		nextPutAll: ' + ';
		print: self rightOffset  @ self bottomOffset ;
		nextPutAll: 'px ) '