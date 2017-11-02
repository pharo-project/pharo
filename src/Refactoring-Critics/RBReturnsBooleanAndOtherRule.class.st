"
This smell arises when a method return a boolean value (true or false) and return some other value such as (nil or self). If the method is suppose to return a boolean, then this signifies that there is one path through the method that might return a non-boolean. If the method doesn''t need to return a boolean, it should be probably rewriten to return some non-boolean value since other programmers reading the method might assume that it returns a boolean.
"
Class {
	#name : #RBReturnsBooleanAndOtherRule,
	#superclass : #RBBlockLintRule,
	#instVars : [
		'matcher'
	],
	#category : 'Refactoring-Critics-BlockRules'
}

{ #category : #testing }
RBReturnsBooleanAndOtherRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBReturnsBooleanAndOtherRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ReturnsBooleanAndOtherRule'
]

{ #category : #running }
RBReturnsBooleanAndOtherRule >> basicCheck: aMethod [ 
	| hasBool hasSelf |
	hasBool := false.
	hasSelf := aMethod parseTree lastIsReturn not.
	(matcher executeTree: aMethod parseTree initialAnswer: Set new) do: [ :each | 
		hasBool := hasBool 
			or: [ (each isLiteralNode and: [ #(true false) includes: each value ]) 
			or: [ (each isMessage and: [ #(and: or:) includes: each selector ]) ] ].
		hasSelf := hasSelf
			or: [ (each isVariable and: [ each name = 'self' ]) 
			or: [ (each isLiteralNode and: [ (#(true false) includes: each value) not ]) ] ] ].
	^ (hasSelf and: [ hasBool ])
]

{ #category : #running }
RBReturnsBooleanAndOtherRule >> checkMethod: aMethod [ 
	(self basicCheck: aMethod) 
		ifTrue: [ result addMethod: aMethod ]
]

{ #category : #accessing }
RBReturnsBooleanAndOtherRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBReturnsBooleanAndOtherRule >> initialize [
	super initialize.
	matcher := RBParseTreeSearcher new.
	matcher 
		matches: '^ ``@object'
		do: [ :node :answer | answer add: node value; yourself ]
]

{ #category : #accessing }
RBReturnsBooleanAndOtherRule >> name [
	^ 'Returns a boolean and non boolean'
]
