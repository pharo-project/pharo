dictionary access
at: aKey put: anObject 
	"Override from Dictionary to check Undeclared and fix up
	references to undeclared variables."
	
	(self includesKey: aKey) ifFalse: 
		[self declare: aKey from: Undeclared.
		self flushClassNameCache].
	super at: aKey put: anObject.
	^ anObject