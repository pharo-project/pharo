preconditions
preconditions
	^(RBCondition isMetaclass: class) not 
		& (RBCondition directlyDefinesClassVariable: variableName asSymbol in: class) 
			& ((RBCondition withBlock: 
						[(#(#Object #Behavior #ClassDescription #Class) includes: class name) not]) 
					errorMacro: 'This refactoring does not work for Object, Behavior, ClassDescription, or Class')