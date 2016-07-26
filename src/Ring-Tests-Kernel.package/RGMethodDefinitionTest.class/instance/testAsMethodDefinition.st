testing
testAsMethodDefinition
	| newMethod |

	newMethod:= (OrderedCollection >> #size) asRingDefinition.
	
	self assert: (newMethod isRingObject).
	self assert: newMethod parent isNil.
	self assert: (newMethod isMethod).  
	self assert: (newMethod selector == #size).  
	self assert: (newMethod protocol notNil).
	self assert: (newMethod stamp notNil).