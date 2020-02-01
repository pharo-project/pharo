"
Check for methods sending the yourself message when it is not necessary.
"
Class {
	#name : #ReYourselfNotUsedRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReYourselfNotUsedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^ 'YourselfNotUsedRule'
]

{ #category : #enumerating }
ReYourselfNotUsedRule >> basicCheck: aNode [
	aNode isMessage ifFalse: [ ^ false ].
	aNode selector = #yourself ifFalse: [ ^ false ].
	^ aNode isUsed not
]

{ #category : #accessing }
ReYourselfNotUsedRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReYourselfNotUsedRule >> name [
	^ 'Doesn''t use the result of a yourself message'
]
