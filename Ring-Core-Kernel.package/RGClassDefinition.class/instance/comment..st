accessing
comment: anObject
	"Sets a RGCommentDefinition object based on the argument"
	
	comment := anObject isRingObject 
		ifFalse: [ 
			RGCommentDefinition new
				parent: self;
				content: anObject; 
				yourself ]
		ifTrue: [anObject ]
	
	 