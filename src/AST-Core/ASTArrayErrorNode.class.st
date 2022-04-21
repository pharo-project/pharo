"
This is a particular englobing node that is an array.
Example : { array node }
Can be created by forgetting 
either the opening : array node }
or the closure : { array node .

"
Class {
	#name : #ASTArrayErrorNode,
	#superclass : #ASTEnglobingErrorNode,
	#category : #'AST-Core-Nodes - ErrorNodes'
}

{ #category : #construction }
ASTArrayErrorNode class >> error: aToken withNodes: aCollection [
	"Analyses the token to identify if it's an opening or a closure."
	| message |
	message := ('{' = aToken value asString) ifTrue: [ '''}'' expected' ] 
										  				 ifFalse: [ '''{'' expected' ].
	"If the collection is empty, there is only the token in the error."
	aCollection isEmpty
		ifTrue: [ ^self new content: aCollection; start: aToken start; stop: aToken stop; errorMessage: message ].
	"If the collection is not empty, we have to sort where the node begins and where it ends."
	^message = '''}'' expected'
		ifTrue: [ self new content: aCollection; start: aToken start; stop: aCollection last stop; errorMessage: message ]
		ifFalse: [ self new content: aCollection; start: aCollection first start; stop: aToken stop; errorMessage: message ]
]

{ #category : #testing }
ASTArrayErrorNode >> isArrayError [
	^true
]

{ #category : #testing }
ASTArrayErrorNode >> repairCollection [
	^ (self errorMessage = '''}'' expected') ifTrue: [ self repairCollectionFromStart ]
													    ifFalse: [ self repairCollectionFromStop ]
]

{ #category : #testing }
ASTArrayErrorNode >> repairCollectionFromStart [
	" The repair process goes through goes through the statements that are compatible with an array.
	  Begins with all statements included in the array and gets one of every time.
	  The removed statements are to be added in the sequence node."
	| collection result removedStatements |
	collection := self content statements.
	result := OrderedCollection new.
	removedStatements := OrderedCollection new.
	[ collection isNotNil ] 
		whileTrue: [ | statements node | 
						 statements := OrderedCollection new.
					    node := ASTArrayNode statements: collection.
						 node start: self start.
						 statements add: node .
						 result add: statements, removedStatements.
						 removedStatements addFirst: collection last.
						 collection removeLast. ].
	result add: removedStatements.
	^collection.
]

{ #category : #testing }
ASTArrayErrorNode >> repairCollectionFromStop [
	" The repair process goes through goes through the statements that are compatible with an array.
	  Begins with all statements included in the array and gets one of every time.
	  The removed statements are to be added in the sequence node."
	| collection result removedStatements |
	collection := self content statements.
	result := OrderedCollection new.
	removedStatements := OrderedCollection new.
	[ collection isNotNil ] 
		whileTrue: [ | statements node | 
						 statements := OrderedCollection new.
						 node := ASTArrayNode statements: collection.
						 node start: self start -1.
						 statements add: ( node ).
						 result add: removedStatements, statements.
						 removedStatements add: collection first.
						 collection removeFirst. ].
	result add: removedStatements.
	^collection.
]
