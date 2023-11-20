"
This is a particular englobing node that is an array.
Exemple : { array node }
Can be created by forgetting 
either the opening : array node }
or the closure : { array node .

"
Class {
	#name : 'RBArrayErrorNode',
	#superclass : 'RBEnglobingErrorNode',
	#category : 'AST-Core-Nodes - ErrorNodes',
	#package : 'AST-Core',
	#tag : 'Nodes - ErrorNodes'
}

{ #category : 'construction' }
RBArrayErrorNode class >> error: aToken withNodes: aCollection [
	"Analyses the token to identify if it's an opening or a closure."
	| message |
	message := ('{' = aToken value asString) ifTrue: [ '''}'' expected' ]
										  				 ifFalse: [ '''{'' expected' ].
	"If the collection is empty, there is only the token in the error."
	aCollection isEmpty
		ifTrue: [ ^self new contents: aCollection; start: aToken start; stop: aToken stop; errorMessage: message ].
	"If the collection is not empty, we have to sort where the node begins and where it ends."
	^message = '''}'' expected'
		ifTrue: [ self new contents: aCollection; start: aToken start; stop: aCollection last stop; errorMessage: message ]
		ifFalse: [ self new contents: aCollection; start: aCollection first start; stop: aToken stop; errorMessage: message ]
]

{ #category : 'testing' }
RBArrayErrorNode >> isArrayError [
	^true
]
