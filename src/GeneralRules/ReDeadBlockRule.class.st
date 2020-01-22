"
Dead Block. The block is not assigned, not returned and no message is send to it.
Often this is a left over from using blocks to comment out code.
"
Class {
	#name : #ReDeadBlockRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReDeadBlockRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'DeadBlockRule'
]

{ #category : #'as yet unclassified' }
ReDeadBlockRule >> basicCheck: node [
	^ node isBlock and: [ node isUsed not ]
]

{ #category : #accessing }
ReDeadBlockRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReDeadBlockRule >> name [
	^ 'Dead Block'
]

{ #category : #accessing }
ReDeadBlockRule >> rationale [
	^ 'Dead Block. The block is not assigned, not returned and no message is send to it.
Often this is a left over from using blocks to comment out code.'
]

{ #category : #accessing }
ReDeadBlockRule >> severity [
	^ #information
]
