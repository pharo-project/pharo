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
			RBRenameInstanceVariableRefactoring
				rename: aVarName
				to: aVarName uncapitalized
				in: aClass).
				
	^ crit