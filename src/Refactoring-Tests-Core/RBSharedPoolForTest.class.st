Class {
	#name : #RBSharedPoolForTest,
	#superclass : #SharedPool,
	#classVars : [
		'Var1'
	],
	#category : #'Refactoring-Tests-Core-Data'
}

{ #category : #accessing }
RBSharedPoolForTest >> msg1 [
	
	^ Var1
]

{ #category : #accessing }
RBSharedPoolForTest >> msg2 [
	
	^ Var1
]
