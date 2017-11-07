"
This smell arises when a method return a boolean value (true or false) and return some other value such as (nil or self). If the method is suppose to return a boolean, then this signifies that there is one path through the method that might return a non-boolean. If the method doesn''t need to return a boolean, it should be probably rewriten to return some non-boolean value since other programmers reading the method might assume that it returns a boolean.
"
Class {
	#name : #RBReturnsBooleanAndOtherRule,
	#superclass : #ReAbstractRule,
	#category : 'GeneralRules-Migrated'
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
RBReturnsBooleanAndOtherRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| returnsBool returnsNonBool |
	returnsBool := false.
	"no return in the end? -> returns self"
	returnsNonBool := aMethod ast lastIsReturn not.
	
	aMethod ast nodesDo: [ :node | 
		node isReturn ifTrue: [
			| returnedNode |
			returnedNode := node value.
			
			returnsBool :=
				returnsBool or:
				[ self checkIfNodeIsBool: returnedNode ].
			returnsNonBool :=
				returnsNonBool or:
				[ self checkIfNodeIsNotBool: returnedNode ].

			returnsBool & returnsNonBool ifTrue: [
				^ aCriticBlock cull: (self critiqueFor: aMethod) ] ] ]
]

{ #category : #private }
RBReturnsBooleanAndOtherRule >> checkIfNodeIsBool: node [
	^ (node isLiteralNode and: [ #(true false) includes: node value ])
		or: [ node isMessage and: [ #(and: or:) includes: node selector ] ]
]

{ #category : #private }
RBReturnsBooleanAndOtherRule >> checkIfNodeIsNotBool: node [
	^ (node isVariable and: [ node name = 'self' ]) or:
		[ node isLiteralNode and: [ (#(true false) includes: node value) not ] ]
]

{ #category : #accessing }
RBReturnsBooleanAndOtherRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBReturnsBooleanAndOtherRule >> name [
	^ 'Returns a boolean and non boolean'
]
