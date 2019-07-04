"
Checks that all temporaries are assigned before they are used. This can help find possible paths through the code where a variable might be unassigned when it is used.
"
Class {
	#name : #RBTempsReadBeforeWrittenRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBTempsReadBeforeWrittenRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBTempsReadBeforeWrittenRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'TempsReadBeforeWrittenRule'
]

{ #category : #running }
RBTempsReadBeforeWrittenRule >> basicCheck: aMethod [
	^ (RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: aMethod ast) isNotEmpty
]

{ #category : #accessing }
RBTempsReadBeforeWrittenRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBTempsReadBeforeWrittenRule >> name [
	^ 'Temporaries read before written'
]
