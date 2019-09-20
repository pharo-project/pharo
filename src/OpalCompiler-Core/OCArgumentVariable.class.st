"
I model argument variables. By definition, an argument variable is always initialized, and can't be written to.
"
Class {
	#name : #OCArgumentVariable,
	#superclass : #OCTempVariable,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
OCArgumentVariable class >> semanticNodeClass [

	^RBArgumentNode 
]

{ #category : #testing }
OCArgumentVariable >> isArg [

	^ true
]

{ #category : #testing }
OCArgumentVariable >> isUninitialized [

	^ false
]

{ #category : #testing }
OCArgumentVariable >> isWritable [

	^ false
]

{ #category : #debugging }
OCArgumentVariable >> writeFromContext: aContext scope: contextScope value: aValue [
	self error: 'Arguments are read only'
]
