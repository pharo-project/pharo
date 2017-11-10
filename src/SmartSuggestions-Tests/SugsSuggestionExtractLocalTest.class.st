"
SUnit tests for extract locale suggestion
"
Class {
	#name : #SugsSuggestionExtractLocalTest,
	#superclass : #SugsSuggestionTest,
	#category : #SmartSuggestions-Tests-Tests
}

{ #category : #'tests - valid' }
SugsSuggestionExtractLocalTest >> testExtractLocalInALiteralNodeIsValid [
	| literal context suggestion |

	literal := RBLiteralValueNode  new.
	context := SugsMockContext withNode: literal.
	suggestion := SugsSuggestionFactory createExtractToLocalVariable context: context.	
	self assert: suggestion isValidForContext .
]

{ #category : #'tests - valid' }
SugsSuggestionExtractLocalTest >> testExtractLocalInAMessageNodeIsValid [
	| context suggestion |
	context := SugsMockContext withMessageName: 'justAName'.
	suggestion := SugsSuggestionFactory createExtractToLocalVariable context: context.
	self assert: suggestion isValidForContext

]

{ #category : #'tests - valid' }
SugsSuggestionExtractLocalTest >> testExtractLocalInASequenceNodeWithMoreSentencesIsNotValid [
	| multipleLines context suggestion |
	
	multipleLines := RBSequenceNode new.
	multipleLines addNode: (RBVariableNode named: 'varName').
	multipleLines addNode: (RBVariableNode named: 'otherName').
	context := SugsMockContext withNode: multipleLines.
	
	suggestion := SugsSuggestionFactory createExtractToLocalVariable context: context.
	
	self deny: suggestion isValidForContext .
	
]

{ #category : #'tests - valid' }
SugsSuggestionExtractLocalTest >> testExtractLocalInASequenceNodeWithOneSentenceIsValid [
	| oneLine context suggestion |
	
	oneLine := RBSequenceNode new.
	oneLine addNode: (RBVariableNode named: 'varName').
	context := SugsMockContext withNode: oneLine.
	
	suggestion := SugsSuggestionFactory createExtractToLocalVariable context: context.
	
	self assert: suggestion isValidForContext .
]
