exceptionDescription
defaultAction
	"The user should be notified of the occurrence of an exceptional occurrence and
	 given an option of continuing or aborting the computation. The description of the
	 occurrence should include any text specified as the argument of the #signal: message."
	
	selector ifNotNil: [self crTrace: class name, '>>', selector, ' ']
			ifNil: [self traceCr:''].
	self traceCr: '(' , name , ' is Undeclared) '.
	^true