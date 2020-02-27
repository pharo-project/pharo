"
I am a rule that has a result. 
It is not clear why it is not merged with its superclass but this is ok too.
"
Class {
	#name : #RBBasicLintRule,
	#superclass : #RBLintRule,
	#instVars : [
		'result'
	],
	#category : #'Refactoring-Critics'
}

{ #category : #accessing }
RBBasicLintRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'BasicLintRule'
]

{ #category : #accessing }
RBBasicLintRule >> filteredResult [
	"Be very careful when filtering results not to introduce new items and not to lose the dedicated browser environments. Try the following steps in order:
	
	- If this is a selector environment use the set-operations of the refactoring browser.
	- If this is a class environment, remove the classes that have a filter annotation in any of its methods.
	- If this is a variable environment, remove the classes and all its variables that have a filter annotation in any of its methods.
	- Otherwise return the unfiltered environment."

	| filter |
	result ifEmpty: [ ^ result ].
	filter := RBPragmaEnvironment
		onEnvironment: RBBrowserEnvironment new
		keywords: #( lint: lint:rationale: lint:rationale:author: lint:author: ignoreLintRule: ignoreLintRule:rationale: ignoreLintRule:rationale:author: ignoreLintRule:author: ).
	filter condition: [ :pragma |
		pragma arguments first = self name
			or: [ pragma arguments first = self group
			or: [ pragma arguments first = self class name ] ] ].
	result isSelectorEnvironment
		ifTrue: [ ^ (result & filter not) label: result label ].
	result isClassEnvironment
		ifTrue: [
			filter classesDo: [ :class |
				result
					removeClass: class classSide;
					removeClass: class instanceSide ] ]
		ifFalse: [
			result isVariableEnvironment ifTrue: [
				filter classesDo: [ :class |
					class classVarNames 
						do: [ :var | result removeClass: class classVariable: var ].
					class instVarNames 
						do: [ :var | result removeClass: class instanceVariable: var ] ] ] ].
	^ result
]

{ #category : #initialization }
RBBasicLintRule >> initialize [
	super initialize.
	self resetResult
	
]

{ #category : #testing }
RBBasicLintRule >> isEmpty [
	^ self result isEmpty
]

{ #category : #accessing }
RBBasicLintRule >> problemCount [
	^ self result problemCount
]

{ #category : #running }
RBBasicLintRule >> resetResult [
	result := self resultClass new.
	result label: self name
]

{ #category : #accessing }
RBBasicLintRule >> result [
	^ result
]

{ #category : #accessing }
RBBasicLintRule >> resultClass [
	self subclassResponsibility
]
