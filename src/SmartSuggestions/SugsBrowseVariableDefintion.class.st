"
I add a menu to variables to browse the defintion
"
Class {
	#name : #SugsBrowseVariableDefintion,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsBrowseVariableDefintion class >> nodes [
	^{RBVariableNode}
]

{ #category : #execution }
SugsBrowseVariableDefintion >> execute [
	| semanticVariable |
	semanticVariable := context selectedNode binding.
	semanticVariable isInstance ifTrue: [ ^semanticVariable slot definingClass browse ].
	semanticVariable isTemp ifTrue: [ ^semanticVariable scope node method browse ].
	semanticVariable isClassVariable ifTrue: [ ^semanticVariable scope getClass browse ].
	semanticVariable isGlobal ifTrue: [ Smalltalk globals inspect ].
]

{ #category : #display }
SugsBrowseVariableDefintion >> label [
	^ 'Browse Variable definition' translated
]
