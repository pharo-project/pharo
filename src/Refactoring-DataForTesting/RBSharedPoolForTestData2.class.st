Class {
	#name : 'RBSharedPoolForTestData2',
	#superclass : 'RBSharedPoolForTestData1',
	#category : 'Refactoring-DataForTesting-ForSharedPool',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForSharedPool'
}

{ #category : 'tests - perform' }
RBSharedPoolForTestData2 >> msg4 [

	"Duplicate here on purpose for testing purposes"
	^ Var1
]

{ #category : 'tests - perform' }
RBSharedPoolForTestData2 >> reference [
	^ RBRemoveClassRefactoringTest
]
