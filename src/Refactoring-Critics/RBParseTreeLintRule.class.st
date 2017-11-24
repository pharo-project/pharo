"
A RBParseTreeLintRule is a rule that is expressed using a parse tree matcher on a method AST.
"
Class {
	#name : #RBParseTreeLintRule,
	#superclass : #RBBasicLintRule,
	#instVars : [
		'matcher'
	],
	#category : #'Refactoring-Critics'
}

{ #category : #testing }
RBParseTreeLintRule class >> checksMethod [
	^ true
]

{ #category : #testing }
RBParseTreeLintRule class >> isVisible [
	^ self name ~= #RBParseTreeLintRule
]

{ #category : #accessing }
RBParseTreeLintRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ParseTreeLintRule'
]

{ #category : #running }
RBParseTreeLintRule >> basicCheck: aMethod [ 
	(self matcher canMatchMethod: aMethod) 
		ifFalse: [ ^ false ].
	^ (self matcher executeTree: aMethod ast initialAnswer: nil) isNotNil
]

{ #category : #running }
RBParseTreeLintRule >> checkMethod: aMethod [ 
	(self basicCheck: aMethod) ifTrue: [ result addMethod: aMethod ]
]

{ #category : #initialization }
RBParseTreeLintRule >> initialize [
	super initialize.
	matcher := RBParseTreeSearcher new
]

{ #category : #accessing }
RBParseTreeLintRule >> matcher [
	^ matcher
]

{ #category : #running }
RBParseTreeLintRule >> resetResult [
	super resetResult.
	self result matcher: self matcher
]

{ #category : #accessing }
RBParseTreeLintRule >> resultClass [
	^ RBParseTreeEnvironment
]
