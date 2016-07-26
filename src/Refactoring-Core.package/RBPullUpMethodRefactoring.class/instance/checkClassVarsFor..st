preconditions
checkClassVarsFor: aSelector 
	class theNonMetaClass classVariableNames do: 
		[ :each | 
		((class whichSelectorsReferToClassVariable: each) includes: aSelector) ifTrue: 
			[ self refactoringError: ('<1p> refers to <2s> which is defined in <3p>' 
					expandMacrosWith: aSelector
					with: each
					with: class) ] ]