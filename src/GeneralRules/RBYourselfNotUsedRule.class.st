"
Check for methods sending the yourself message when it is not necessary.
"
Class {
	#name : #RBYourselfNotUsedRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBYourselfNotUsedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^ 'YourselfNotUsedRule'
]

{ #category : #enumerating }
RBYourselfNotUsedRule >> check: aNode forCritiquesDo: aCriticBlock [
	aNode isMessage ifFalse: [ ^ self ].
	aNode selector = #yourself ifFalse: [ ^ self ].
	aNode isUsed ifFalse: [ 
		aCriticBlock cull: (
			self critiqueFor: aNode) ]
]

{ #category : #accessing }
RBYourselfNotUsedRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
RBYourselfNotUsedRule >> name [
	^ 'Doesn''t use the result of a yourself message'
]
