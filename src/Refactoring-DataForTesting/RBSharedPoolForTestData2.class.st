Class {
	#name : 'RBSharedPoolForTestData2',
	#superclass : 'RBSharedPoolForTestData1',
	#category : 'Refactoring-DataForTesting-ForSharedPool',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForSharedPool'
}

{ #category : 'tests - perform' }
RBSharedPoolForTestData2 >> msg4 [

	^ Var1
]

{ #category : 'tests - perform' }
RBSharedPoolForTestData2 >> reference [
	^ RBRemoveClassRefactoringTest
]
