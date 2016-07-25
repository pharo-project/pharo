tests-class slots
testWithClassSlots

	aClass := self make: [ :builder | 
		builder classSlots: #(a) ].
	
	self assert: aClass class instVarNames equals: #(a).	
	aClass instVarNamed: #a put: $A.
	self assert: (aClass instVarNamed: #a) equals: $A.