accessing
stamp: aString

	self hasComment
	ifTrue: [ self comment stamp: aString ]
	ifFalse:[ RGCommentDefinition new
						 parent: self; 
						 stamp: aString ]