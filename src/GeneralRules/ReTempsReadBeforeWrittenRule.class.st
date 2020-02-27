"
Checks that all temporaries are assigned before they are used. This can help find possible paths through the code where a variable might be unassigned when it is used.
"
Class {
	#name : #ReTempsReadBeforeWrittenRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReTempsReadBeforeWrittenRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReTempsReadBeforeWrittenRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'TempsReadBeforeWrittenRule'
]

{ #category : #running }
ReTempsReadBeforeWrittenRule >> basicCheck: aMethod [
	^ (RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: aMethod ast) isNotEmpty
]

{ #category : #accessing }
ReTempsReadBeforeWrittenRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
ReTempsReadBeforeWrittenRule >> name [
	^ 'Temporaries read before written'
]
