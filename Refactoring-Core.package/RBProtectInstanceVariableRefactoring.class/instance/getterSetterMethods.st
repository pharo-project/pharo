private-accessing
getterSetterMethods
	| matcher |
	matcher := RBParseTreeSearcher new.
	matcher
		answer: Set new;
		matchesAnyMethodOf: (Array 
					with: '`method ^' , variableName
					with: ('`method: `arg <1s> := `arg' expandMacrosWith: variableName)
					with: ('`method: `arg ^<1s> := `arg' expandMacrosWith: variableName))
			do: 
				[:aNode :answer | 
				(class subclassRedefines: aNode selector) 
					ifFalse: [answer add: aNode selector].
				answer].
	(class whichSelectorsReferToInstanceVariable: variableName) do: 
			[:each | 
			self 
				checkClass: class
				selector: each
				using: matcher].
	^matcher answer