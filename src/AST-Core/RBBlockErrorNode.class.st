"
This is a particular englobing node that is a Block.
Exemple : [ block node ]
Can be created by forgetting 
either the opening : block node ]
or the closure : [ block node .

"
Class {
	#name : 'RBBlockErrorNode',
	#superclass : 'RBEnglobingErrorNode',
	#instVars : [
		'arguments'
	],
	#category : 'AST-Core-Nodes - ErrorNodes',
	#package : 'AST-Core',
	#tag : 'Nodes - ErrorNodes'
}

{ #category : 'construction' }
RBBlockErrorNode class >> error: aToken withNodes: aCollection [
	"Analyses the token to identify if it's an opening or a closure."
	| message |
	message := ('[' = aToken value asString) ifTrue: [ ''']'' expected' ]
										  				 ifFalse: [ '''['' expected' ].
	"If the collection is empty, there is only the token in the error."
	aCollection isEmpty
		ifTrue: [ ^self new contents: aCollection; start: aToken start; stop: aToken stop; errorMessage: message ].
	"If the collection is not empty, we have to sort where the node begins and where it ends."
	^message = ''']'' expected'
		ifTrue: [ self new contents: aCollection; start: aToken start; stop: aCollection last stop; errorMessage: message ]
		ifFalse: [ self new contents: aCollection; start: aCollection first start; stop: aToken stop; errorMessage: message ]
]

{ #category : 'accessing' }
RBBlockErrorNode >> argumentNames [

	^ self arguments collect: [:each | each name] as: Array
]

{ #category : 'accessing' }
RBBlockErrorNode >> arguments [
	^arguments
]

{ #category : 'accessing' }
RBBlockErrorNode >> arguments: anObject [

	arguments := anObject
]

{ #category : 'testing' }
RBBlockErrorNode >> isBlockError [
	^true
]
