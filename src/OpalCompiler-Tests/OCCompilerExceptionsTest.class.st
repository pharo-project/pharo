Class {
	#name : #OCCompilerExceptionsTest,
	#superclass : #TestCase,
	#instVars : [
		'text',
		'currentCompiler'
	],
	#category : #'OpalCompiler-Tests-FromOld'
}

{ #category : #compiling }
OCCompilerExceptionsTest >> compile: sourceString [

       text := sourceString.
       self class
               compileSilently: text
               classified: 'generated'
               notifying: self
]

{ #category : #compiling }
OCCompilerExceptionsTest >> compiling: sourceCode shouldRaise: exceptionClass [

	self should: [ self compile: sourceCode ] raise: exceptionClass
]

{ #category : #compiling }
OCCompilerExceptionsTest >> interactive [ 
	^ true
]

{ #category : #compiling }
OCCompilerExceptionsTest >> removeGeneratedMethods [

	self class removeProtocol: 'generated'
]

{ #category : #emulating }
OCCompilerExceptionsTest >> select [
	
]

{ #category : #emulating }
OCCompilerExceptionsTest >> selectFrom: start to: end [ 
	
]

{ #category : #emulating }
OCCompilerExceptionsTest >> selectionInterval [
	^ 1 to: 0
]

{ #category : #running }
OCCompilerExceptionsTest >> setUp [
	super setUp.
	currentCompiler := SmalltalkImage compilerClass.
	SmalltalkImage compilerClass: OpalCompiler.

]

{ #category : #running }
OCCompilerExceptionsTest >> tearDown [
	self removeGeneratedMethods.
	SmalltalkImage compilerClass: currentCompiler.
	super tearDown
]

{ #category : #tests }
OCCompilerExceptionsTest >> testUndeclaredVariable [

	self
		compiling: 'griffle ^ goo' shouldRaise: OCUndeclaredVariableWarning  ;
		compiling: 'griffle ^ [ goo ] value' shouldRaise: OCUndeclaredVariableWarning
]

{ #category : #emulating }
OCCompilerExceptionsTest >> text [
	^ text
]
