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
TemporaryVariable >> acceptVisitor: arg1 node: arg2 [

	^ arg1 visitTemporaryVariableNode: arg2
]

{ #category : 'queries' }
TemporaryVariable >> definingNode [

	^ scope node temporaries
		  detect: [ :arg1 | arg1 variable == self ]
		  ifNone: [  ]
]

{ #category : 'testing' }
TemporaryVariable >> isTempVariable [

	^ true
]
