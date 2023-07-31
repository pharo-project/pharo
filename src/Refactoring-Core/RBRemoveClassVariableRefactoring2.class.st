Class {
	#name : #RBRemoveClassVariableRefactoring2,
	#superclass : #RBVariableRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #preconditions }
RBRemoveClassVariableRefactoring2 >> applicabilityConditions [

	^ ( RBCondition isMetaclass: class ) not
		& ( RBCondition definesClassVariable: variableName in: class )

]

{ #category : #preconditions }
RBRemoveClassVariableRefactoring2 >> breakingChangeConditions [

	^ ( RBCondition
				withBlock: [ | block |

					block := [ :each |
					( each whichSelectorsReferToClassVariable: variableName ) isEmpty
						ifFalse: [ class realClass
								ifNil: [ self refactoringError: ( '<1s> is referenced.' expandMacrosWith: variableName ). ]
								ifNotNil: [ self
										refactoringWarning: ( '<1s> is referenced.<n>Browse references?' expandMacrosWith: variableName )
										with: [ self
												openBrowserOn:
													( RBVariableEnvironment referencesToClassVariable: variableName in: class realClass )
											]
									]
							]
					].
					class withAllSubclasses do: block.
					class classSide withAllSubclasses do: block.
					true
					] )
]

{ #category : #actions }
RBRemoveClassVariableRefactoring2 >> generateChanges [
	
	self applicabilityConditions check ifFalse: [
		^ RBApplicabilityChecksFailedError signal:
			  self applicabilityConditions errorString ].
	self breakingChangeConditions check ifFalse: [
		RBBreakingChangeChecksFailedWarning signal:
			self breakingChangeConditions errorString ].

	self transform.
	^ self changes
]

{ #category : #preconditions }
RBRemoveClassVariableRefactoring2 >> preconditions [

	^ self applicabilityConditions & self breakingChangeConditions 
]

{ #category : #transforming }
RBRemoveClassVariableRefactoring2 >> transform [
	class removeClassVariable: variableName
]
