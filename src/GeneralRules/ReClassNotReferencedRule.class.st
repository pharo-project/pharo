"
This smell arises when a class is not referenced either directly or indirectly by a symbol. If a class is not referenced, it can be removed.
"
Class {
	#name : #ReClassNotReferencedRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReClassNotReferencedRule class >> checksClass [
	^ true
]

{ #category : #accessing }
ReClassNotReferencedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ClassNotReferencedRule'
]

{ #category : #enumerating }
ReClassNotReferencedRule >> basicCheck: aClass [
	^ aClass isUsed not
]

{ #category : #accessing }
ReClassNotReferencedRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
ReClassNotReferencedRule >> name [
	^ 'Class not referenced'
]

{ #category : #accessing }
ReClassNotReferencedRule >> rationale [
	^ 'Check if a class is referenced either directly or indirectly by a symbol. If a class is not referenced, it can be removed.'
]

{ #category : #accessing }
ReClassNotReferencedRule >> severity [
	^ #information
]
