tests - file in/out
testOrdersMetaClassAfterItsClassInstance
	| ordered |
	ordered := (Class superclassOrder:  
		(OrderedCollection 
				with: Boolean class
				with: True
				with: Boolean
				with: True class)).
	
	self assert: (ordered indexOf: Boolean) < (ordered indexOf: Boolean class).
	self assert: (ordered indexOf: True) < (ordered indexOf: True class).
	self assert: (ordered indexOf: Boolean class) < (ordered indexOf: True class).
	self assert: (ordered indexOf: Boolean) < (ordered indexOf: True).
