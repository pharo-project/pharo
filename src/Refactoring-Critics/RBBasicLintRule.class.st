"
I am a rule that has a result. 
It is not clear why it is not merged with its superclass but this is ok too.
"
Class {
	#name : 'RBBasicLintRule',
	#superclass : 'RBLintRule',
	#instVars : [
		'result'
	],
	#category : 'Refactoring-Critics',
	#package : 'Refactoring-Critics'
}

{ #category : 'accessing' }
RBBasicLintRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'BasicLintRule'
]

{ #category : 'initialization' }
RBBasicLintRule >> initialize [
	super initialize.
	self resetResult
]

{ #category : 'testing' }
RBBasicLintRule >> isEmpty [
	^ self result isEmpty
]

{ #category : 'accessing' }
RBBasicLintRule >> problemCount [
	^ self result problemCount
]

{ #category : 'running' }
RBBasicLintRule >> resetResult [
	result := self resultClass new.
	result label: self name
]

{ #category : 'accessing' }
RBBasicLintRule >> result [
	^ result
]

{ #category : 'accessing' }
RBBasicLintRule >> resultClass [
	self subclassResponsibility
]
