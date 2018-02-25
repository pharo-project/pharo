"
The classic option to extract a line of code in a local variable
"
Class {
	#name : #SugsExtractToLocalVariable,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsExtractToLocalVariable class >> nodes [
	^{RBSequenceNode. RBMessageNode. RBLiteralNode . RBBlockNode }
]

{ #category : #execution }
SugsExtractToLocalVariable >> execute [
	(NautilusRefactoring model: context) extractToTempTextSelection
]

{ #category : #display }
SugsExtractToLocalVariable >> icon [
	^self iconNamed: #smallJumpIcon
]

{ #category : #execution }
SugsExtractToLocalVariable >> isValidForContext [
	^context hasOneStatement.
]

{ #category : #display }
SugsExtractToLocalVariable >> label [
	^ 'Extract local' translated
]

{ #category : #display }
SugsExtractToLocalVariable >> position [
	^20
]
