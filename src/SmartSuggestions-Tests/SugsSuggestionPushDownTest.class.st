"
SUnit test for the push down suggestions
"
Class {
	#name : #SugsSuggestionPushDownTest,
	#superclass : #SugsSuggestionTest,
	#category : #'SmartSuggestions-Tests-Tests'
}

{ #category : #private }
SugsSuggestionPushDownTest >> createContext: subclass [
	
	^SugsMockContext for: subclass .
]

{ #category : #private }
SugsSuggestionPushDownTest >> createSubclassFrom: superClass named: symbol [
	| newClass |
	
	newClass := superClass 
					subclass: symbol
					instanceVariableNames: ''
					classVariableNames: ''
					poolDictionaries: ''
					category: 'SmartSuggestions-Test'.
	^newClass.
]

{ #category : #private }
SugsSuggestionPushDownTest >> createSuggestionPushDown: context [
	
	^ SugsSuggestionFactory createPushDown 
		context:context;
		yourself.
]

{ #category : #'tests - methods' }
SugsSuggestionPushDownTest >> testPushDownWithSubclassIsValid [
	| suggestion subclass |

	subclass := self createSubclassFrom: self emptyClass named: #TestSuggestionsSubclass.
	self addMethodToDelegate.
	suggestion := self createSuggestionPushDown: (self createContext: self emptyClass ).
	
	self assert: suggestion isValidForContext.
	self removeClass: subclass.
]

{ #category : #'tests - methods' }
SugsSuggestionPushDownTest >> testPushDownWithoutSubclassIsNotValid [
	| suggestion |

	self addMethodToDelegate.
	suggestion := self createSuggestionPushDown: (self createContext: self emptyClass ).
	
	self deny: suggestion isValidForContext
]
