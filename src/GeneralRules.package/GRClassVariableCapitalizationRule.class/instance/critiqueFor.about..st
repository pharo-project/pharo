running-helpers
critiqueFor: aClass about: aVarName
	| crit |

	crit := ReRefactoringCritique
		withAnchor: (ReVarSearchSourceAnchor
			entity: aClass
			string: aVarName)
		by: self.
	
	crit	
		tinyHint: aVarName;
		refactoring: (
			RBRenameClassVariableRefactoring
				rename: aVarName
				to: aVarName capitalized
				in: aClass).
				
	^ crit