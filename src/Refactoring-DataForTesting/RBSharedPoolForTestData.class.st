Class {
	#name : 'RBSharedPoolForTestData',
	#superclass : 'SharedPool',
	#classVars : [
		'Var1'
	],
	#category : 'Refactoring-DataForTesting-ForSharedPool',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForSharedPool'
}

{ #category : 'tests - perform' }
RBSharedPoolForTestData >> msg1 [

	^ Var1
]

{ #category : 'tests - perform' }
RBSharedPoolForTestData >> msg2 [

	^ Var1
]

{ #category : 'tests - perform' }
RBSharedPoolForTestData >> msg4 [

	^ Var1
]
