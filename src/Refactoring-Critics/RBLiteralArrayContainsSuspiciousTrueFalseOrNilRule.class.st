"
Some times ago, arrays were not allowed to contain true false and nil objects. They only contain their symbol representation: evaluating #(true false nil) returns #(#true #false #nil). 

Nowadays, #(true false nil) is equivalent to {true . false . nil }, i.e., it returns an array with the objects true, false, and nil. 

This smells checks methods having #(#true #false #nil) in their literal frame since it can be the source of potential bugs. 
	 
"
Class {
	#name : #RBLiteralArrayContainsSuspiciousTrueFalseOrNilRule,
	#superclass : #RBBlockLintRule,
	#instVars : [
		'matcher'
	],
	#category : 'Refactoring-Critics-BlockRules'
}

{ #category : #testing }
RBLiteralArrayContainsSuspiciousTrueFalseOrNilRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBLiteralArrayContainsSuspiciousTrueFalseOrNilRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'LiteralArrayContainsSuspiciousTrueFalseOrNilRule'
]

{ #category : #running }
RBLiteralArrayContainsSuspiciousTrueFalseOrNilRule >> basicCheck: aMethod [ 
	| compiledLits parsedLits |
	compiledLits := aMethod allLiterals
		inject: OrderedCollection new
		into: [ :collection :literal |
			collection
				addAll: (self literalTrueFalseOrNilSymbolsIn: literal);
				yourself ].
	compiledLits size strictlyPositive ifTrue: [ 
		parsedLits := OrderedCollection new.
		matcher 
			executeTree: aMethod ast
			initialAnswer: parsedLits.
		^ compiledLits size ~= parsedLits size ].
	
	^ false
]

{ #category : #running }
RBLiteralArrayContainsSuspiciousTrueFalseOrNilRule >> checkMethod: aMethod [ 
	(self basicCheck: aMethod) ifTrue: [
			result addMethod: aMethod ]
]

{ #category : #accessing }
RBLiteralArrayContainsSuspiciousTrueFalseOrNilRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBLiteralArrayContainsSuspiciousTrueFalseOrNilRule >> initialize [
	super initialize.
	matcher := RBParseTreeSearcher new.
	matcher 
		matches: '`#array `{ :node | node isLiteralArray and: [ node isForByteArray not ] }'
		do: [ :node :answer | 
			answer 
				addAll: (self literalTrueFalseOrNilSymbolsIn: node value);
				yourself ]
]

{ #category : #private }
RBLiteralArrayContainsSuspiciousTrueFalseOrNilRule >> literalTrueFalseOrNilSymbolsIn: aLiteral [ 
	| retval |
	aLiteral class == Array ifFalse: [ ^ #() ].
	retval := OrderedCollection withAll: (aLiteral 
		select: [ :each | each isSymbol and: [ #(#true #false #nil ) includes: each ] ]).
	aLiteral do: [ :each | retval addAll: (self literalTrueFalseOrNilSymbolsIn: each) ].
	^ retval
]

{ #category : #accessing }
RBLiteralArrayContainsSuspiciousTrueFalseOrNilRule >> name [
	^ 'Literal array contains a #true, #false, or #nil but the source doesn''t.'
]

{ #category : #accessing }
RBLiteralArrayContainsSuspiciousTrueFalseOrNilRule >> rationale [
	^ '#(true false nil) now is equal to {true. false. nil} not {#true. #false. #nil} as it used to be.'
]
