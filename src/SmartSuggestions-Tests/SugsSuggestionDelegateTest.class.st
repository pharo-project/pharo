"
SUnit tests for smart suggestions delegates
"
Class {
	#name : #SugsSuggestionDelegateTest,
	#superclass : #SugsSuggestionTest,
	#category : 'SmartSuggestions-Tests-Tests'
}

{ #category : #private }
SugsSuggestionDelegateTest >> createContexForDelegate [
	^ SugsMockContext for: self emptyClass
]

{ #category : #private }
SugsSuggestionDelegateTest >> createSuggestionDelegate: context [
	| suggestion |

	suggestion  := SugsSuggestionFactory createMoveMethod .
	suggestion context: context.
	^suggestion.
	

]

{ #category : #'tests - source' }
SugsSuggestionDelegateTest >> testDelegateSelectionWithInstaceVariablesItsValid [
	| suggestion |

	self addVariable: 'test' to: self emptyClass.
	self addMethodToDelegate.
	suggestion := self createSuggestionDelegate: self createContexForDelegate .
	
	self assert: suggestion isValidForContext
]

{ #category : #'tests - source' }
SugsSuggestionDelegateTest >> testDelegateSelectionWithoutInstaceVariablesItsNotValid [
	| suggestion |
	
	self addMethodToDelegate.
	suggestion := self createSuggestionDelegate: self createContexForDelegate.
	
	self deny: suggestion isValidForContext
]
