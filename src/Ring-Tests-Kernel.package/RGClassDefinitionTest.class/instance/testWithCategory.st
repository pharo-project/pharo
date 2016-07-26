testing
testWithCategory
	| newClass |
	
	newClass:= (RGClassDefinition named:  #Object)
						category: 'Kernel-Objects';
						yourself.

	self assert: (newClass package isNil).
	self assert: (newClass category = 'Kernel-Objects').