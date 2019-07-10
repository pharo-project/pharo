"
This smell arises when a class is not referenced either directly or indirectly by a symbol. If a class is not referenced, it can be removed.
"
Class {
	#name : #RBClassNotReferencedRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBClassNotReferencedRule class >> checksClass [
	^ true
]

{ #category : #accessing }
RBClassNotReferencedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ClassNotReferencedRule'
]

{ #category : #enumerating }
RBClassNotReferencedRule >> basicCheck: aClass [
	^ aClass isUsed not
]

{ #category : #accessing }
RBClassNotReferencedRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
RBClassNotReferencedRule >> name [
	^ 'Class not referenced'
]

{ #category : #accessing }
RBClassNotReferencedRule >> rationale [
	^ 'Check if a class is referenced either directly or indirectly by a symbol. If a class is not referenced, it can be removed.'
]

{ #category : #accessing }
RBClassNotReferencedRule >> severity [
	^ #information
]
