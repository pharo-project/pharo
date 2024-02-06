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
	self assert: (PoolDefiner class hasBindingThatBeginsWith: 'AnAu')
]

{ #category : 'tests' }
MetaClassTest >> testHasClassVarNamed [

	self assert: (Character class hasClassVarNamed: #CharSet).
	self deny: (Character class hasClassVarNamed: #NotCharSet)
]

{ #category : 'tests' }
MetaClassTest >> testclassVarNames [

	self assert: (Character class classVarNames includes: #CharSet).

	"A class and it's meta-class share the class variables"
	self assert: Character class classVarNames equals: Character class classVarNames
]
