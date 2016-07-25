preconditions
preconditions
	| references |
	references := RBCondition referencesInstanceVariable: variableName
				in: class.
	class realClass isNil 
		ifTrue: 
			[references 
				errorMacro: ('<1s> is referenced.' expandMacrosWith: variableName)]
		ifFalse: 
			[references
				errorMacro: ('<1s> is referenced.<n>Browse references?' 
							expandMacrosWith: variableName);
				errorBlock: 
						[self openBrowserOn: (RBBrowserEnvironment new instVarRefsTo: variableName
									in: class realClass)]].
	^(RBCondition definesInstanceVariable: variableName in: class) 
		& references not