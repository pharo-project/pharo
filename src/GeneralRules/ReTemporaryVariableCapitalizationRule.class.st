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
	^ (self checkIsVariableWithParent: aNode) and: [ ((self checkIsArgumentDefinition: aNode) or: [ self checkIsTempDefinition: aNode ]) and: [ aNode name first isUppercase ] ]
]

{ #category : #'running-helpers' }
ReTemporaryVariableCapitalizationRule >> checkIsArgumentDefinition: aNode [

	^ aNode isArgument and: [ aNode parent isMethod ]
]

{ #category : #'running-helpers' }
ReTemporaryVariableCapitalizationRule >> checkIsTempDefinition: aNode [

	^ aNode isTemp and: [ aNode parent isSequence ]
]

{ #category : #'running-helpers' }
ReTemporaryVariableCapitalizationRule >> checkIsVariableWithParent: aNode [

	^ aNode isVariable and: [
	  aNode parent isNotNil ]
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
			RBRenameTemporaryRefactoring
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
