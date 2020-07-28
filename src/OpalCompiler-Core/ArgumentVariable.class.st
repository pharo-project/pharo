"
I model argument variables. By definition, an argument variable is always initialized, and can't be written to.
"
Class {
	#name : #ArgumentVariable,
	#superclass : #LocalVariable,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
ArgumentVariable class >> semanticNodeClass [

	^RBArgumentNode 
]

{ #category : #accessing }
ArgumentVariable >> copiedVarClass [
	^OCCopyingArgumentVariable
]

{ #category : #queries }
ArgumentVariable >> definingNode [
	^ scope node arguments detect: [ :each | each name = name ]
]

{ #category : #testing }
ArgumentVariable >> isArgumentVariable [
	^ true
]

{ #category : #testing }
ArgumentVariable >> isUninitialized [

	^ false
]

{ #category : #testing }
ArgumentVariable >> isWritable [

	^ false
]

{ #category : #debugging }
ArgumentVariable >> writeFromContext: aContext scope: contextScope value: aValue [
	self error: 'Arguments are read only'
]
