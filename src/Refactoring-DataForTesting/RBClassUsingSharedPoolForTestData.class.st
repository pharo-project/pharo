Class {
	#name : 'RBClassUsingSharedPoolForTestData',
	#superclass : 'Object',
	#traits : 'RBTDummy',
	#classTraits : 'RBTDummy classTrait',
	#pools : [
		'RBSharedPoolForTestData'
	],
	#category : 'Refactoring-DataForTesting-ForSharedPool',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForSharedPool'
}

{ #category : 'accessing - instances and variables' }
RBClassUsingSharedPoolForTestData >> msg1 [

	var1 asString
]

{ #category : 'accessing - instances and variables' }
RBClassUsingSharedPoolForTestData >> msg2 [

	var1
]

{ #category : 'accessing - instances and variables' }
RBClassUsingSharedPoolForTestData >> msg3 [

	^ Var1
]
