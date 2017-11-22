"
This rule checks for method that wrongly initialize an object twice. Contrary to other Smalltalk implementations Pharo automatically calls #initiailize on object creation.
A warning is raised when the statement self new initialize is found in a method.
"
Class {
	#name : #RBSuperSendsNewRule,
	#superclass : #RBBlockLintRule,
	#instVars : [
		'matcher'
	],
	#category : #Refactoring-Critics-BlockRules
}

{ #category : #testing }
RBSuperSendsNewRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBSuperSendsNewRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SuperSendsNewRule'
]

{ #category : #running }
RBSuperSendsNewRule >> basicCheck: aMethod [ 
	aMethod methodClass isMeta ifTrue: [ ^ false ].
	^ (matcher executeTree: aMethod ast initialAnswer: false) 
		
]

{ #category : #running }
RBSuperSendsNewRule >> checkMethod: aMethod [ 
	(self basicCheck: aMethod) 
		ifTrue: [ result addMethod: aMethod ]
]

{ #category : #accessing }
RBSuperSendsNewRule >> group [
	^ 'Bugs'
]

{ #category : #initialization }
RBSuperSendsNewRule >> initialize [
	super initialize.
	matcher := RBParseTreeSearcher new.
	matcher 
		matchesAnyOf: #(
			'super new initialize'
			'(super new: `@expr) initialize'
			'self new initialize'
			'(self new: `@expr) initialize' )
		do: [ :answer :node | true ].
]

{ #category : #accessing }
RBSuperSendsNewRule >> name [
	^ 'Sends super new initialize'
]

{ #category : #accessing }
RBSuperSendsNewRule >> rationale [
	^ 'Checks for methods that wrongly initialize an object twice. Contrary to other Smalltalk implementations Pharo automatically calls #initiailize on object creation.'
]
