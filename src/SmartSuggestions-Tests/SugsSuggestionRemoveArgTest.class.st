"
SUnit tests for the remove argument suggestion
"
Class {
	#name : #SugsSuggestionRemoveArgTest,
	#superclass : #SugsSuggestionTest,
	#category : #'SmartSuggestions-Tests-Tests'
}

{ #category : #'tests - methods' }
SugsSuggestionRemoveArgTest >> testRemoveArgsWithArgsItsValid [
	| suggestion |

	suggestion := SugsSuggestionFactory createRemoveParameter context: (SugsMockContext withMessageName: 'methodNoParamas:'  ); yourself.
	
	self assert: suggestion isValidForContext
	
]

{ #category : #'tests - methods' }
SugsSuggestionRemoveArgTest >> testRemoveArgsWithoutArgsItsNotValid [
	| suggestion |

	suggestion := SugsSuggestionFactory createRemoveParameter context: (SugsMockContext withMessageName: 'methodNoParamas' ); yourself.
		
	self deny: suggestion isValidForContext
]
