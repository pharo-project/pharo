Class {
	#name : 'TestClass',
	#superclass : 'TestRootClass',
	#category : 'Kernel-CodeModel-Tests-Support',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Support'
}

{ #category : 'support' }
TestClass class >> classSideMethodUsingInheritedSomeShareVariable [
	^ SomeSharedVariable 
]

{ #category : 'support' }
TestClass >> anotherMethodUsingSomeSharedVariable [
	^ SomeSharedVariable
]

{ #category : 'support' }
TestClass >> methodUsingInheritedSharedPoolVariable [
	^ One
]
