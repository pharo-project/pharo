"
This smell arises when a class is not referenced either directly or indirectly by a symbol. If a class is not referenced, it can be removed.
"
Class {
	#name : 'ReClassNotReferencedRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Design Flaws',
	#package : 'General-Rules',
	#tag : 'Design Flaws'
}

{ #category : 'testing' }
ReClassNotReferencedRule class >> checksClass [
	^ true
]

{ #category : 'accessing' }
ReClassNotReferencedRule class >> group [
	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReClassNotReferencedRule class >> rationale [
	^ 'Check if a class is referenced either directly or indirectly by a symbol. If a class is not referenced, it can be removed.'
]

{ #category : 'accessing' }
ReClassNotReferencedRule class >> ruleName [
	^ 'Class not referenced'
]

{ #category : 'accessing' }
ReClassNotReferencedRule class >> severity [
	^ #information
]

{ #category : 'accessing' }
ReClassNotReferencedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'ClassNotReferencedRule'
]

{ #category : 'enumerating' }
ReClassNotReferencedRule >> basicCheck: aClass [
	^ aClass isUsed not
]
