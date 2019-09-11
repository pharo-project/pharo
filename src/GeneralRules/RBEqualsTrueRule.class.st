"
Check for a =, ==, ~=, or ~~ message being sent to true/false or with true/false as the argument. Many times these can be eliminated since their receivers are already booleans. For example, ""anObject isFoo == false"" could be replaced with ""anObject isFoo not"" if isFoo always returns a boolean. Sometimes variables might refer to true, false, and something else, but this is considered bad style since the variable has multiple types.
"
Class {
	#name : #RBEqualsTrueRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBEqualsTrueRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'EqualsTrueRule'
]

{ #category : #running }
RBEqualsTrueRule >> basicCheck: aNode [
	| parent |
	aNode isLiteralNode ifFalse: [ ^ false ].
	(#(true false) includes: aNode value) ifFalse: [ ^ false ].
	(parent := aNode parent) ifNil: [ ^ false ].
	parent isMessage ifFalse: [ ^ false ].
	^ #(#= #== #~= #~~) includes: parent selector
]

{ #category : #accessing }
RBEqualsTrueRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
RBEqualsTrueRule >> name [
	^ 'Unnecessary "= true"'
]

{ #category : #accessing }
RBEqualsTrueRule >> severity [
	^ #information
]
