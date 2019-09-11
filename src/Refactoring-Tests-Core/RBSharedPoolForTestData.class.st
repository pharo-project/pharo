Class {
	#name : #RBSharedPoolForTestData,
	#superclass : #SharedPool,
	#classVars : [
		'Var1'
	],
	#category : #'Refactoring-Tests-Core-Data'
}

{ #category : #'tests-perform' }
RBSharedPoolForTestData >> msg1 [
	
	^ Var1
]

{ #category : #'tests-perform' }
RBSharedPoolForTestData >> msg2 [
	
	^ Var1
]

{ #category : #'tests-perform' }
RBSharedPoolForTestData >> msg4 [
	
	^ Var1
]
