tests-class slots
testSwitchClassSlotAndMigrate

	| classAdded classModified |	
	classAdded := 
		self make: [ :builder | 
			builder classSlots: #(a b) ].

	"Add methods"	
	classAdded class 
		compile: 'a ^a';
		compile: 'b ^b'.

	"Put values"
	classAdded instVarNamed: #a put: $A.
	classAdded instVarNamed: #b put: $B.

	"Modify the class"	
	classModified := 
		self make: [ :builder | 
			builder classSlots: #(b a) ].
	
	"The instance was migrated"
	self assert: (classModified instVarNamed: #a) equals: $A.
	self assert: (classModified instVarNamed: #b) equals: $B.
	
	"The methods were migrated"
	self assert: classModified a equals: $A.
	self assert: classModified b equals: $B.
