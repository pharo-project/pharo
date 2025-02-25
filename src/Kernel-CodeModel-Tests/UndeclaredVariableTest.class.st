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

	class1 compile: 'm 
	  ^ TestClass2'.
	
	self deny: ((class1 >> #m) literals includes: #runtimeUndeclaredReadInContext:)
]

{ #category : 'tests' }
UndeclaredVariableTest >> testUsingUndeclaredVariableCompilesMessageSendToVariable [

	| class1 |
	
	class1 := self newClass: #TestClass1.
	class1 compile: 'm 
	  ^ TestClass2'.
	
	self assert: ((class1 >> #m) literals includes: #runtimeUndeclaredReadInContext:)
]

{ #category : 'tests' }
UndeclaredVariableTest >> testUsingUndeclaredVariableInABlockIsRecompiledWhenDefiningTheUndeclaredVariable [

	| class1 |
	
	class1 := self newClass: #TestClass1.
	class1 compile: 'm 
	  ^ [TestClass2]'.

	self newClass: #TestClass2.
		
	self deny: (((class1 >> #m) literalAt: 1) literals includes: #runtimeUndeclaredReadInContext:)
]

{ #category : 'tests' }
UndeclaredVariableTest >> testUsingUndeclaredVariableIsRecompiledWhenDefiningTheUndeclaredVariable [

	| class1 |
	
	class1 := self newClass: #TestClass1.
	class1 compile: 'm 
	  ^ TestClass2'.

	self newClass: #TestClass2.
		
	self deny: ((class1 >> #m) literals includes: #runtimeUndeclaredReadInContext:)
]
