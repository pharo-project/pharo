"
A SugsSelectedValidCodeAsMethod the action needed to extract the selection to another method in the same class.
THe difference between SugsExtracMethod is that this actually uses the user selection.
In fact we should replace the SugsExtracMethod  with this one.

"
Class {
	#name : #SugsExtractStatements,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsExtractStatements >> execute [
	| selectedValidCodeInterval |
	selectedValidCodeInterval := self privateCalculateInterval.
	(NautilusRefactoring model: context) extractBetween: selectedValidCodeInterval from: context selectedMethod
]

{ #category : #display }
SugsExtractStatements >> icon [
	^self iconNamed: #smallUpdateIcon
]

{ #category : #display }
SugsExtractStatements >> label [
	^'Extract method' translated.
	

]

{ #category : #display }
SugsExtractStatements >> position [
	^10
]

{ #category : #private }
SugsExtractStatements >> privateCalculateInterval [
	| selectedValidCodeInterval selectedValidCodeStatements |
	selectedValidCodeStatements := context selectedStatementsAsSequence.
	selectedValidCodeInterval := selectedValidCodeStatements start to: selectedValidCodeStatements stop.
	^ selectedValidCodeInterval
]
