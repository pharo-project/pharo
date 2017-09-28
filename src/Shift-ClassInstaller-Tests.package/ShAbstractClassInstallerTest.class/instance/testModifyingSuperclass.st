tests
testModifyingSuperclass
	superClass := self newClass:#ShCITestClass slots:#(anInstanceVariable).
	newClass := self newClass: #ShCITestSubClass superclass: superClass slots: #().
	
	superClass new instVarNamed: #anInstanceVariable.
	newClass new instVarNamed: #anInstanceVariable.	
	
	superClass := self newClass:#ShCITestClass slots:#(anInstanceVariable otherVariable).

	superClass new instVarNamed: #anInstanceVariable.
	newClass new instVarNamed: #anInstanceVariable.	
	superClass new instVarNamed: #otherVariable.
	newClass new instVarNamed: #otherVariable.	
