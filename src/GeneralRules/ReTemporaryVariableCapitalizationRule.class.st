"
Temporary and argument variable names should start with a lowercase letter.
"
Class {
	#name : #ReTemporaryVariableCapitalizationRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #running }
ReTemporaryVariableCapitalizationRule >> basicCheck: aNode [
	aNode isVariable ifFalse: [ ^false ].
	aNode isLocalVariable ifFalse: [ ^false ].
	aNode isDefinition ifFalse: [ ^false ].
	^aNode name first isUppercase
]

{ #category : #helpers }
ReTemporaryVariableCapitalizationRule >> critiqueFor: aNode [
	| crit |

	crit := ReRefactoringCritique
		withAnchor: (self anchorFor: aNode)
		by: self.
		
	crit	
		tinyHint: aNode name;
		refactoring: (
			RBRenameArgumentOrTemporaryRefactoring
				renameTemporaryFrom: aNode sourceInterval
				to: aNode name uncapitalized
				in: aNode methodNode methodClass
				selector: aNode methodNode selector).
				
	^ crit	
]

{ #category : #accessing }
ReTemporaryVariableCapitalizationRule >> group [
	^ 'Style'
]

{ #category : #accessing }
ReTemporaryVariableCapitalizationRule >> name [
	^ 'Temporary variable (or parameter) capitalized'
]
