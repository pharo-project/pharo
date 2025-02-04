Class {
	#name : 'RBSharedPoolForTestData1',
	#superclass : 'RBSharedPoolForTestData',
	#category : 'Refactoring-DataForTesting-ForSharedPool',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForSharedPool'
}

{ #category : 'tests perform' }
RBSharedPoolForTestData1 >> msg4 [

	"Duplicate here on purpose for testing purposes"
	^ Var1
]
