accessing
backgroundColor 
	^backgroundColor ifNil: [ 
		backgroundColor := (Smalltalk ui theme settings menuColor ifNil: [ Color white ]) ]