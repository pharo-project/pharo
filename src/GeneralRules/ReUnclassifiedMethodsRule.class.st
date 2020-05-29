"
All methods should be put into a protocol (method category) for better readability.
"
Class {
	#name : #ReUnclassifiedMethodsRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReUnclassifiedMethodsRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReUnclassifiedMethodsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnclassifiedMethodsRule'
]

{ #category : #running }
ReUnclassifiedMethodsRule >> basicCheck: aMethod [
	^ aMethod protocol = Protocol unclassified
]

{ #category : #accessing }
ReUnclassifiedMethodsRule >> group [
	^ 'Style'
]

{ #category : #accessing }
ReUnclassifiedMethodsRule >> name [
	^ 'Unclassified methods'
]
