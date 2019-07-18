"
I am a refactoring for removing class variables.

My precondition verifies that there is no reference to this class variable.
"
Class {
	#name : #RBRemoveClassVariableRefactoring,
	#superclass : #RBVariableRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #preconditions }
RBRemoveClassVariableRefactoring >> preconditions [

	^ ( RBCondition isMetaclass: class ) not
		& ( RBCondition definesClassVariable: variableName in: class )
		&
			( RBCondition
				withBlock: [ | block |

					block := [ :each | 
					( each whichSelectorsReferToClassVariable: variableName ) isEmpty
						ifFalse: [ class realClass
								ifNil: [ self refactoringError: ( '<1s> is referenced.' expandMacrosWith: variableName ) ]
								ifNotNil: [ self
										refactoringError: ( '<1s> is referenced.<n>Browse references?' expandMacrosWith: variableName )
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

{ #category : #transforming }
RBRemoveClassVariableRefactoring >> transform [
	class removeClassVariable: variableName
]
