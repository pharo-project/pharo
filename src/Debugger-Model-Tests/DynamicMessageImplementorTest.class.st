"
SUnit tests for class DynamicMessageImplementor
"
Class {
	#name : #DynamicMessageImplementorTest,
	#superclass : #TestCase,
	#instVars : [
		'instVar'
	],
	#category : #'Debugger-Model-Tests-Core'
}

{ #category : #accessing }
DynamicMessageImplementorTest >> instVar [
	"Only required so that instVar is referenced as we do not want unreferenced ivars in the image"
	
	^ instVar
]

{ #category : #testing }
DynamicMessageImplementorTest >> testBinaryMessage [

	| messageImplementor sourceCode |		
	
	messageImplementor := DynamicMessageImplementor for: (Message selector: #+ argument: 1) in: self class.	
	sourceCode := messageImplementor value.	
	self assert: sourceCode withSeparatorsCompacted equals: '+ anInteger self shouldBeImplemented.' withSeparatorsCompacted
]

{ #category : #testing }
DynamicMessageImplementorTest >> testConflictingArgumentNames [

	| messageImplementor sourceCode |		
	
	messageImplementor := DynamicMessageImplementor for: (Message selector: #m1:m2:m3: arguments: #(1 2 3)) in: self class.	
	sourceCode := messageImplementor value.	
	self assert: sourceCode withSeparatorsCompacted equals: 'm1: anInteger m2: anInteger2 m3: anInteger3 self shouldBeImplemented.' withSeparatorsCompacted
]

{ #category : #testing }
DynamicMessageImplementorTest >> testGenerateBooleanArgumentNameForFalse [

	| messageImplementor sourceCode |		
	
	messageImplementor := DynamicMessageImplementor for: (Message selector: #iReceiveABoolean: arguments: #(false)) in: self class.
	sourceCode := messageImplementor value.
	self assert: sourceCode withSeparatorsCompacted equals: 'iReceiveABoolean: aBoolean self shouldBeImplemented.' withSeparatorsCompacted
]

{ #category : #testing }
DynamicMessageImplementorTest >> testGenerateBooleanArgumentNameForTrue [

	| messageImplementor sourceCode |		
	
	messageImplementor := DynamicMessageImplementor for: (Message selector: #iReceiveABoolean: arguments: #(true)) in: self class.
	sourceCode := messageImplementor value.
	self assert: sourceCode withSeparatorsCompacted equals: 'iReceiveABoolean: aBoolean self shouldBeImplemented.' withSeparatorsCompacted
]

{ #category : #testing }
DynamicMessageImplementorTest >> testGenerateGetter [

	| messageImplementor sourceCode |		
	
	messageImplementor := DynamicMessageImplementor 
		for: (Message selector: #instVar) 
		in: self class.	
	sourceCode := messageImplementor value.	
	self 
		assert: sourceCode withSeparatorsCompacted 
		equals: 'instVar	^ instVar' withSeparatorsCompacted
]

{ #category : #testing }
DynamicMessageImplementorTest >> testGenerateSetter [

	| messageImplementor sourceCode |		
	
	messageImplementor := DynamicMessageImplementor
		for: (Message selector: #instVar: argument: 1)
		in: self class.	
	sourceCode := messageImplementor value.	
	self
		assert: sourceCode withSeparatorsCompacted
		equals: 'instVar: anInteger instVar := anInteger' withSeparatorsCompacted
]

{ #category : #testing }
DynamicMessageImplementorTest >> testNonConflictingArgumentNames [

	| messageImplementor sourceCode |		
	
	messageImplementor := DynamicMessageImplementor for: (Message selector: #m1:m2:m3: arguments: #(1 $a 'string')) in: self class.	
	sourceCode := messageImplementor value.	
	self assert: sourceCode withSeparatorsCompacted equals: 'm1: anInteger m2: aCharacter m3: aString self shouldBeImplemented.' withSeparatorsCompacted
]

{ #category : #testing }
DynamicMessageImplementorTest >> testOneArgumentNotMatchingAnInstanceVariable [

	| messageImplementor sourceCode |		
	
	messageImplementor := DynamicMessageImplementor for: (Message selector: #m1: argument: 1) in: self class.	
	sourceCode := messageImplementor value.	
	self assert: sourceCode withSeparatorsCompacted equals: 'm1: anInteger self shouldBeImplemented.' withSeparatorsCompacted
]

{ #category : #testing }
DynamicMessageImplementorTest >> testUnaryMessageNotMatchingAnInstanceVariable [

	| messageImplementor sourceCode |		
	
	messageImplementor := DynamicMessageImplementor for: (Message selector: #m1) in: self class.	
	sourceCode := messageImplementor value.	
	self assert: sourceCode withSeparatorsCompacted equals: 'm1 self shouldBeImplemented.' withSeparatorsCompacted
]
