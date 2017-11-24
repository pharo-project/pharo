"
Check for string concatenation inside some iteration message. Since string concatenation is O(n^2), it is better to use streaming since it is O(n) - assuming that n is large enough. As a general principal avoid , since the receiver is copied. Therefore chaining , messages will lead to multiple useless copies of the receiver. 

Instead of writing
	| string | 
	string := String new.
	#(1 2 3) do: [ :each |
		string := string, each asString].
	^ string

Write, it is much more efficient.

	String streamContents: [:s | 
		#(1 2 3)  do: [:each | s nextPutAll: each asString]]
	
or more concisely...
	'''' join: {1. 1+1. 3}	

"
Class {
	#name : #RBStringConcatenationRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBStringConcatenationRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'StringConcatenationRule'
]

{ #category : #hooks }
RBStringConcatenationRule >> afterCheck: aNode mappings: mappingDict [
	(self performsConcatenation: (mappingDict at: '``@argument'))
		ifTrue: [ ^ true ].
		
	(mappingDict
		at: '``@argument2'
		ifPresent: [ :arg2 |
			^ self performsConcatenation: arg2 ]).
		
	^ false 
]

{ #category : #accessing }
RBStringConcatenationRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBStringConcatenationRule >> initialize [
	super initialize.
	self  matchesAny: #(
		'``@collection do: ``@argument' 
		'``@collection do: ``@argument separatedBy: ``@argument2' 
		'``@start to: ``@stop do: ``@argument' 
		'``@collection detect: ``@argument' 
		'``@collection detect: ``@argument ifNone: ``@argument2' 
		'``@collection select: ``@argument'
		'``@collection reject: ``@argument' 
		'``@collection inject: ``@value into: ``@argument'
		'``@collection anySatisfy: ``@argument'
		'``@collection allSatisfy: ``@argument'
		'``@collection noneSatisfy: ``@argument' )
]

{ #category : #accessing }
RBStringConcatenationRule >> name [
	^ 'String concatenation instead of streams'
]

{ #category : #hooks }
RBStringConcatenationRule >> performsConcatenation: aNode [

	aNode isBlock ifFalse: [ ^ false ].
	aNode nodesDo: [ :node |
		(node isMessage and: [ 
		node selector = #, ]) ifTrue: [ 
			^ true ] ].
	
	^ false
]
