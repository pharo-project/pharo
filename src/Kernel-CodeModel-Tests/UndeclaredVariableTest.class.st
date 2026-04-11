Class {
	#name : 'UndeclaredVariableTest',
	#superclass : 'TestCase',
	#instVars : [
		'createdClasses'
	],
	#category : 'Kernel-CodeModel-Tests-UndeclaredVariables',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'UndeclaredVariables'
}

{ #category : 'tests' }
UndeclaredVariableTest >> compile: source in: class [

	| method compiler |
	compiler := class compiler
		            source: source;
		            permitUndeclared: true;
		            permitFaulty: false.
	method := compiler compile.
	class methodInstaller install: method into: class compiler: compiler.
	^ method selector
]

{ #category : 'instance creation' }
UndeclaredVariableTest >> newClass: aName [

	^ createdClasses add: (self class classInstaller make: [ :aClassBuilder |
		  aClassBuilder
			  name: aName;
			  package: self class package name ]).
]

{ #category : 'running' }
UndeclaredVariableTest >> setUp [ 

	super setUp.
	createdClasses := OrderedCollection new.
]

{ #category : 'running' }
UndeclaredVariableTest >> tearDown [ 

	createdClasses reversed do: [ :aClass | aClass removeFromSystem ]. 
	super tearDown.

]

{ #category : 'tests' }
UndeclaredVariableTest >> testUsingDeclaredVariableCompilesUsingVariable [

	| class1 |
	class1 := self newClass: #TestClass1.
	self newClass: #TestClass2.

	self
		compile: 'm 
	  ^ TestClass2'
		in: class1.

	self deny: ((class1 >> #m) literals includes: #runtimeUndeclaredReadInContext:)
]

{ #category : 'tests' }
UndeclaredVariableTest >> testUsingUndeclaredVariableCompilesMessageSendToVariable [

	| class1 |
	class1 := self newClass: #TestClass1.
	self
		compile: 'm 
	  ^ TestClass2'
		in: class1.

	self assert: ((class1 >> #m) literals includes: #runtimeUndeclaredReadInContext:)
]

{ #category : 'tests' }
UndeclaredVariableTest >> testUsingUndeclaredVariableInABlockIsRecompiledWhenDefiningTheUndeclaredVariable [

	| class1 |
	class1 := self newClass: #TestClass1.
	self
		compile: 'm 
	  ^ [TestClass2]'
		in: class1.

	self newClass: #TestClass2.

	self deny: ((class1 >> #m literalAt: 1) literals includes: #runtimeUndeclaredReadInContext:)
]

{ #category : 'tests' }
UndeclaredVariableTest >> testUsingUndeclaredVariableIsRecompiledWhenDefiningTheUndeclaredVariable [

	| class1 |
	class1 := self newClass: #TestClass1.
	self
		compile: 'm 
	  ^ TestClass2'
		in: class1.

	self newClass: #TestClass2.

	self deny: ((class1 >> #m) literals includes: #runtimeUndeclaredReadInContext:)
]
