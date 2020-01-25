"
Some times ago, arrays were not allowed to contain true false and nil objects. They only contain their symbol representation: evaluating #(true false nil) returns #(#true #false #nil). 

Nowadays, #(true false nil) is equivalent to {true . false . nil }, i.e., it returns an array with the objects true, false, and nil. 

This smells checks methods having #(#true #false #nil) in their literal frame since it can be the source of potential bugs. 
	 
"
Class {
	#name : #ReLiteralArrayContainsSuspiciousTrueFalseOrNilRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReLiteralArrayContainsSuspiciousTrueFalseOrNilRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'LiteralArrayContainsSuspiciousTrueFalseOrNilRule'
]

{ #category : #running }
ReLiteralArrayContainsSuspiciousTrueFalseOrNilRule >> basicCheck: aNode [
	^ aNode isLiteralArray and: [ aNode value includesAny: #(#true #false #nil) ]
]

{ #category : #accessing }
ReLiteralArrayContainsSuspiciousTrueFalseOrNilRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
ReLiteralArrayContainsSuspiciousTrueFalseOrNilRule >> name [
	^ 'Literal array contains a #true, #false, or #nil.'
]

{ #category : #accessing }
ReLiteralArrayContainsSuspiciousTrueFalseOrNilRule >> severity [
	^ #information
]
