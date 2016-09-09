helpers
critiqueFor: aNode
	| crit |

	crit := ReRefactoringCritique
		withAnchor: (self anchorFor: aNode)
		by: self.
		
	crit	
		tinyHint: aNode name;
		refactoring: (
			RBRenameTemporaryRefactoring
				renameTemporaryFrom: aNode sourceInterval
				to: aNode name uncapitalized
				in: aNode methodNode methodClass
				selector: aNode methodNode selector).
				
	^ crit	