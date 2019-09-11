Class {
	#name : #RBSharedPoolForTestData2,
	#superclass : #RBSharedPoolForTestData1,
	#category : #'Refactoring-Tests-Core-Data'
}

{ #category : #'tests-perform' }
RBSharedPoolForTestData2 >> msg4 [
	
	^ Var1
]

{ #category : #'tests-perform' }
RBSharedPoolForTestData2 >> reference [ 	
	^ RBRemoveClassTest
]
