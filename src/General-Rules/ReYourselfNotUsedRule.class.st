"
Check for methods sending the yourself message when it is not necessary.
"
Class {
	#name : 'ReYourselfNotUsedRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
ReYourselfNotUsedRule class >> group [
	^ 'Optimization'
]

{ #category : 'accessing' }
ReYourselfNotUsedRule class >> ruleName [
	^ 'Doesn''t use the result of a yourself message'
]

{ #category : 'accessing' }
ReYourselfNotUsedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^ 'YourselfNotUsedRule'
]

{ #category : 'enumerating' }
ReYourselfNotUsedRule >> basicCheck: aNode [
	aNode isMessage ifFalse: [ ^ false ].
	aNode selector = #yourself ifFalse: [ ^ false ].
	^ aNode isUsedAsReturnValue not
]
