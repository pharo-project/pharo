"
I add a menu to variables to browse the defintion
"
Class {
	#name : #SugsBrowseVariableDefinition,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsBrowseVariableDefinition class >> nodes [
	^{RBVariableNode}
]

{ #category : #execution }
SugsBrowseVariableDefinition >> execute [
	| semanticVariable |
	semanticVariable := context selectedNode binding.
	semanticVariable isInstance ifTrue: [ ^semanticVariable slot definingClass browse ].
	semanticVariable isTemp ifTrue: [ ^semanticVariable scope node method browse ].
	semanticVariable isClassVariable ifTrue: [ ^semanticVariable scope getClass browse ].
	semanticVariable isGlobal ifTrue: [ Smalltalk globals inspect ].
]

{ #category : #display }
SugsBrowseVariableDefinition >> label [
	^ 'Browse Variable definition' translated
]
