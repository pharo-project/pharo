"
Tests related to SharedPool
"
Class {
	#name : 'SharedPoolTest',
	#superclass : 'TestCase',
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'tests' }
SharedPoolTest >> testIncludesKey [

	self assert: (PoolDefiner includesKey: #AnAuthor).
	self deny: (PoolDefiner includesKey: #NonExistingVariableNotEvenByChance)
]

{ #category : 'tests' }
SharedPoolTest >> testMethodsAccessingPoolVariablesDoNotFindPoolUsers [

	| result |
	result := PoolDefiner methodsAccessingPoolVariables.

	"but we do not include references to the pool itself"
	self deny: (result includes:
			 SharedPoolTest
			 >> #testMethodsAccessingPoolVariablesDoNotFindPoolUsers)
]

{ #category : 'tests' }
SharedPoolTest >> testMethodsAccessingPoolVariablesFindsVariableUsers [

	| result |
	result := PoolDefiner methodsAccessingPoolVariables.

	"we find accesses to the vars of the shared pool"
	self assert: (result includes: PoolDefiner class >> #initialize)
]

{ #category : 'tests' }
SharedPoolTest >> testPoolUsers [

	| result |
	result := PoolDefiner poolUsers.
	self assertCollection: result hasSameElements: {
			ClassMultiplePoolUser.
			ExampleForTestWithSharedPool.
			RootClassPoolUser }
]

{ #category : 'tests' }
SharedPoolTest >> testUsingMethodsFindsPoolUsers [

	| result |
	result := PoolDefiner usingMethods.

	self assert: (result includes:
			 SharedPoolTest
			 >> #testMethodsAccessingPoolVariablesDoNotFindPoolUsers)
]

{ #category : 'tests' }
SharedPoolTest >> testUsingMethodsFindsVariableUsers [

	| result |
	result := PoolDefiner usingMethods.

	"we find accesses to the vars of the shared pool"
	self assert: (result includes: PoolDefiner class >> #initialize)
]
