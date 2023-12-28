Class {
	#name : 'MetaClassTest',
	#superclass : 'TestCase',
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'tests' }
MetaClassTest >> testHasBindingThatBeginsWith [
	self assert: (SmalltalkImage class hasBindingThatBeginsWith: 'Compiler').
	self assert: (SmalltalkImage class hasBindingThatBeginsWith: 'Object').

	"Pools are looked up, too"
	self assert: (TimeZone class hasBindingThatBeginsWith: 'DaysInMo')
]

{ #category : 'tests' }
MetaClassTest >> testHasClassVarNamed [

	self assert: (Object class hasClassVarNamed: #DependentsFields).
	self deny: (Object class hasClassVarNamed: #CompilerClass)
]

{ #category : 'tests' }
MetaClassTest >> testclassVarNames [

	self assert: (Object class classVarNames includes: #DependentsFields).

	"A class and it's meta-class share the class variables"
	self assert: Object class classVarNames equals: Object classVarNames
]
