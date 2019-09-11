"
Temporary and argument variable names should start with a lowercase letter.
"
Class {
	#name : #GRTemporaryVariableCapitalizationRule,
	#superclass : #ReNodeBasedRule,
	#category : #GeneralRules
}

{ #category : #running }
GRTemporaryVariableCapitalizationRule >> basicCheck: aNode [
	^ (self checkIsVariableWithParent: aNode) and: [ ((self checkIsArgumentDefinition: aNode) or: [ self checkIsTempDefinition: aNode ]) and: [ aNode name first isUppercase ] ]
]

{ #category : #'running-helpers' }
GRTemporaryVariableCapitalizationRule >> checkIsArgumentDefinition: aNode [

	^ aNode isArgument and: [ aNode parent isMethod ]
]

{ #category : #'running-helpers' }
GRTemporaryVariableCapitalizationRule >> checkIsTempDefinition: aNode [

	^ aNode isTemp and: [ aNode parent isSequence ]
]

{ #category : #'running-helpers' }
GRTemporaryVariableCapitalizationRule >> checkIsVariableWithParent: aNode [

	^ aNode isVariable and: [
	  aNode parent isNotNil ]
]

{ #category : #helpers }
GRTemporaryVariableCapitalizationRule >> critiqueFor: aNode [
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
GRTemporaryVariableCapitalizationRule >> group [
	^ 'Style'
]

{ #category : #accessing }
GRTemporaryVariableCapitalizationRule >> name [
	^ 'Temporary variable (or parameter) capitalized'
]
