"
Temporary and argument variable names should start with a lowercase letter.
"
Class {
	#name : 'ReTemporaryVariableCapitalizationRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Style',
	#package : 'General-Rules',
	#tag : 'Style'
}

{ #category : 'accessing' }
ReTemporaryVariableCapitalizationRule class >> group [
	^ 'Style'
]

{ #category : 'accessing' }
ReTemporaryVariableCapitalizationRule class >> ruleName [
	^ 'Temporary variable (or parameter) capitalized'
]

{ #category : 'running' }
ReTemporaryVariableCapitalizationRule >> basicCheck: aNode [
	aNode isLocalVariable ifFalse: [ ^false ].
	aNode isDefinition ifFalse: [ ^false ].
	^aNode name first isUppercase
]

{ #category : 'helpers' }
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
