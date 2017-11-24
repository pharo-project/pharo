"
I am an acceptor in the context of method definition
"
Class {
	#name : #MethodDefinitionAcceptor,
	#superclass : #AbstractAcceptor,
	#category : #'Nautilus-Acceptors'
}

{ #category : #protocol }
MethodDefinitionAcceptor >> accept: source notifying: aController [
	| protocol method |

	protocol := self model selectedCategory.
	method := self model selectedMethod.
	
	protocol ifNil: [
		protocol := method 
			ifNil: [ Protocol unclassified ]
			ifNotNil: [ method protocol ] ].

	self model
		compileAMethodFromCategory: protocol
		withSource: source
		notifying: aController
]
