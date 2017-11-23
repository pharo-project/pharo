"
The base rule for smalltalk code pattern matching (relies on rewrite expressions). The rule operates on AST nodes.

Use the following methods in the initialization to setup your subclass:

- matches:
- addMatchingExpression:
	add a string of rewrite expression to be matched by rule

- matchesAny:
	same as previous but takes a collection of strings to match
	
- addMatchingMethod:
	add a string of rewrite expression which should be parsed as a method
	
you may use #afterCheck:mappings: to do a post-matching validation of a matched node and mapping of wildcards.
"
Class {
	#name : #ReNodeMatchRule,
	#superclass : #RePatternCodeRule,
	#instVars : [
		'matchers'
	],
	#category : #'Renraku-Rules-Rewrite'
}

{ #category : #testing }
ReNodeMatchRule class >> isVisible [

	^ self name ~= #ReNodeMatchRule
]

{ #category : #adding }
ReNodeMatchRule >> addMatchingExpression: aString [
	"add a string of rewrite expression to be matched by rule"

	matchers add: (RBParser parseRewriteExpression: aString)
]

{ #category : #adding }
ReNodeMatchRule >> addMatchingMethod: aString [
	"add a string of rewrite expression which should be parsed as a method"

	matchers add: (RBParser parseRewriteMethod: aString)
]

{ #category : #running }
ReNodeMatchRule >> check: aNode forCritiquesDo: aCriticBlock [
	
	matchers do: [ :matcher |
		matcher
			match: aNode
			onSuccess: [ :map |
				(self
					afterCheck: aNode
					mappings: (self flattenMatchMap: map))
						ifTrue: [
							aCriticBlock cull: (self critiqueFor: aNode) ] ]
			onFailure: [  ] ]
	
]

{ #category : #initialization }
ReNodeMatchRule >> initialize [
	super initialize.
	matchers := Set new.
]

{ #category : #adding }
ReNodeMatchRule >> matches: aString [
	"add a string of rewrite expression to be matched by rule"

	self addMatchingExpression: aString
]

{ #category : #adding }
ReNodeMatchRule >> matchesAny: aStringCollection [
	"add a collection of rewrite expression strings to be matched by the rule"

	aStringCollection do: [ :string |
		self addMatchingExpression: string ]
]
