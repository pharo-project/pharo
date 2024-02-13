"
Checks for senders of comparator messages that do not use the result of the comparison.
"
Class {
	#name : 'ReEqualNotUsedRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'accessing' }
ReEqualNotUsedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'EqualNotUsedRule'
]

{ #category : 'running' }
ReEqualNotUsedRule >> basicCheck: node [
	^ node isMessage and: [ node isUsedAsReturnValue not and: [ #(#= #== #~= #~~ #< #> #<= #>=) includes: node selector ] ]
]

{ #category : 'accessing' }
ReEqualNotUsedRule >> group [
	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReEqualNotUsedRule >> name [
	^ 'Doesn''t use the result of a =, ~=, etc.'
]
