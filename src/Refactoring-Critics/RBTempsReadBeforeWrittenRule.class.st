"
Checks that all temporaries are assigned before they are used. This can help find possible paths through the code where a variable might be unassigned when it is used.
"
Class {
	#name : #RBTempsReadBeforeWrittenRule,
	#superclass : #RBBlockLintRule,
	#category : 'Refactoring-Critics-BlockRules'
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
	| a |
	a asString.
	a := 1.
	^ (RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: aMethod ast) notEmpty 
]

{ #category : #running }
RBTempsReadBeforeWrittenRule >> checkMethod: aMethod [ 
	(RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: aMethod ast) do: 
		[ :each | 
		result 
			addMethod: aMethod;
			addSearchString: each ]
]

{ #category : #accessing }
RBTempsReadBeforeWrittenRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBTempsReadBeforeWrittenRule >> name [
	^ 'Temporaries read before written'
]
