accessing
numberOfRows 
	^ self elements 
		ifNotNil: [ self elements size ]
		ifNil: [ 0 ]