"
Checks that all temporaries are assigned before they are used. This can help find possible paths through the code where a variable might be unassigned when it is used.

The rule detects, for example, when the assignment is done in a block that might not be executed. 

exampleMethod
	| temp |
	self doSomething: [ temp := 1].
	^temp.

As the rule can not know about the semantics of the method doSomething:, it assumes the block will not
be exected.

NOTE: This rule detects *many* false positives.
"
Class {
	#name : #ReTempsReadBeforeWrittenRule,
	#superclass : #ReAbstractRule,
	#instVars : [
		'temps'
	],
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
	self temps: ((RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: aMethod ast) array select: #isNotNil).
	^ temps isNotEmpty
]

{ #category : #accessing }
ReTempsReadBeforeWrittenRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
ReTempsReadBeforeWrittenRule >> name [

	^ String streamContents: [ :stream |
		  stream nextPutAll: 'Temporaries may be read before written ('.
		  self temps
			  do: [ :temporary | stream nextPutAll: temporary ]
			  separatedBy: [ stream nextPutAll: ', ' ].
		  stream nextPut: $) ]
]

{ #category : #accessing }
ReTempsReadBeforeWrittenRule >> temps [
	^ temps
]

{ #category : #accessing }
ReTempsReadBeforeWrittenRule >> temps: anArray [
	temps:= anArray
]
