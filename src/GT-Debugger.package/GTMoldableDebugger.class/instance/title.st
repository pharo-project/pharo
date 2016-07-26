accessing
title
	
	^ super title 
		ifNil: [ self class defaultTitle ] 
		ifNotNil: [ :aString | aString ] 