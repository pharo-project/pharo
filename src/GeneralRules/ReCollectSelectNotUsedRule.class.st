"
Checks for senders of typical collection enumeration methods that return an unused result.

For example, check that the result of a select: or detect: is used.
"
Class {
	#name : #ReCollectSelectNotUsedRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReCollectSelectNotUsedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'CollectSelectNotUsedRule'
]

{ #category : #running }
ReCollectSelectNotUsedRule >> basicCheck: aNode [
	aNode isMessage ifFalse: [ ^ false ].
	(#(#select: #collect: #reject:) includes: aNode selector) ifFalse: [ ^ false ].
	^ aNode isUsed not
]

{ #category : #accessing }
ReCollectSelectNotUsedRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReCollectSelectNotUsedRule >> name [
	^ 'Doesn''t use the result of a collect:/select:'
]
