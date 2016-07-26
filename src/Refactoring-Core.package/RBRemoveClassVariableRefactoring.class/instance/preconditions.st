preconditions
preconditions
	^ (RBCondition isMetaclass: class) not & (RBCondition 
			definesClassVariable: variableName
			in: class) & (RBCondition withBlock: 
			[ | block |
			block := 
			[ :each | 
			(each whichSelectorsReferToClassVariable: variableName) isEmpty ifFalse: 
				[ class realClass isNil 
					ifTrue: 
						[ self refactoringError: ('<1s> is referenced.' expandMacrosWith: variableName) ]
					ifFalse: 
						[ self 
							refactoringError: ('<1s> is referenced.<n>Browse references?' expandMacrosWith: variableName)
							with: 
								[ self openBrowserOn: (RBVariableEnvironment 
										referencesToClassVariable: variableName
										in: class realClass) ] ] ] ].
			class withAllSubclasses do: block.
			class theMetaClass withAllSubclasses do: block.
			true ])