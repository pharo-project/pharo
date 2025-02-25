"
I model temp variables. With Closures, there are two kinds: Copying and those that are stored in a so called temp vector, a heap allocated array that itself is stored in a copying temp variable.
"
Class {
	#name : 'TemporaryVariable',
	#superclass : 'LocalVariable',
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'visiting' }
TemporaryVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitTemporaryVariableNode: aNode
]

{ #category : 'queries' }
TemporaryVariable >> definingNode [
	^ scope node temporaries
		detect: [ :each | each variable == self ]
		ifNone: [ nil ]
]

{ #category : 'testing' }
TemporaryVariable >> isTempVariable [

	^ true
]
