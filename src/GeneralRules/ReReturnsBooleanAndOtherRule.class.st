"
This smell arises when a method return a boolean value (true or false) and return some other value such as (nil or self). If the method is suppose to return a boolean, then this signifies that there is one path through the method that might return a non-boolean. If the method doesn''t need to return a boolean, it should be probably rewriten to return some non-boolean value since other programmers reading the method might assume that it returns a boolean.
"
Class {
	#name : #ReReturnsBooleanAndOtherRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReReturnsBooleanAndOtherRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReReturnsBooleanAndOtherRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ReturnsBooleanAndOtherRule'
]

{ #category : #running }
ReReturnsBooleanAndOtherRule >> check: aMethod forCritiquesDo: aCriticBlock [
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
ReReturnsBooleanAndOtherRule >> checkIfNodeIsBool: node [
	^ (node isLiteralNode and: [ #(true false) includes: node value ])
		or: [ node isMessage and: [ #(and: or:) includes: node selector ] ]
]

{ #category : #private }
ReReturnsBooleanAndOtherRule >> checkIfNodeIsNotBool: node [
	^ node isSelfVariable or:
		[ node isLiteralNode and: [ (#(true false) includes: node value) not ] ]
]

{ #category : #accessing }
ReReturnsBooleanAndOtherRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
ReReturnsBooleanAndOtherRule >> name [
	^ 'Returns a boolean and non boolean'
]
