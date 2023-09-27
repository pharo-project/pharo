"
I model argument variables. By definition, an argument variable is always initialized, and can't be written to.
"
Class {
	#name : 'ArgumentVariable',
	#superclass : 'LocalVariable',
	#category : 'OpalCompiler-Core-Semantics',
	#package : 'OpalCompiler-Core',
	#tag : 'Semantics'
}

{ #category : 'visiting' }
ArgumentVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitArgumentVariableNode: aNode
]

{ #category : 'queries' }
ArgumentVariable >> definingNode [
	^ scope node arguments detect: [ :each | each variable == self ]
]

{ #category : 'testing' }
ArgumentVariable >> isArgumentVariable [
	^ true
]

{ #category : 'testing' }
ArgumentVariable >> isUninitialized [

	^ false
]

{ #category : 'testing' }
ArgumentVariable >> isWritable [

	^ false
]

{ #category : 'debugging' }
ArgumentVariable >> writeFromContext: aContext scope: contextScope value: aValue [
	self error: 'Arguments are read only'
]
