"
Checks for senders of typical collection enumeration methods that return an unused result.

For example, check that the result of a select: or detect: is used.
"
Class {
	#name : #RBCollectSelectNotUsedRule,
	#superclass : #ReNodeBasedRule,
	#category : #GeneralRules-Migrated
}

{ #category : #accessing }
RBCollectSelectNotUsedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'CollectSelectNotUsedRule'
]

{ #category : #running }
RBCollectSelectNotUsedRule >> check: aNode forCritiquesDo: aCritiqueBlock [


	aNode isMessage ifFalse: [ ^ self ].
	(#(#select: #collect: #reject:) includes: aNode selector) ifFalse: [ ^ self ].
	aNode isUsed ifTrue: [ ^ self ].
	
	aCritiqueBlock cull: ( self critiqueFor: aNode )
]

{ #category : #accessing }
RBCollectSelectNotUsedRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
RBCollectSelectNotUsedRule >> name [
	^ 'Doesn''t use the result of a collect:/select:'
]
