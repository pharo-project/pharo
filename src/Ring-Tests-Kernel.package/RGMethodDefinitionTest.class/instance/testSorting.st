testing
testSorting

	| rgMethod1 rgMethod2 |
	rgMethod1 := RGMethodDefinition realClass: RGInstanceVariableDefinition selector: #isInstanceVariable.
	rgMethod2 := RGMethodDefinition realClass: RGElementDefinition selector: #isMetaSide.
	self assert: rgMethod2 <= rgMethod1.
	
	rgMethod1 := RGMethodDefinition realClass: RGElementDefinition selector: #parentName.
	self assert: rgMethod2 <= rgMethod1. 
	