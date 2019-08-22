Class {
	#name : #RBSharedPoolForTestData,
	#superclass : #SharedPool,
	#classVars : [
		'Var1'
	],
	#category : #'Refactoring-Tests-Core-Data'
}

{ #category : #accessing }
RBSharedPoolForTestData >> msg1 [
	
	^ Var1
]

{ #category : #accessing }
RBSharedPoolForTestData >> msg2 [
	
	^ Var1
]

{ #category : #'as yet unclassified' }
RBSharedPoolForTestData >> msg4 [
	
	^ Var1
]
