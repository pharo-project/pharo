Class {
	#name : #RBClassToRename,
	#superclass : #Model,
	#category : #'Refactoring-DataForTesting-MiniHierarchy'
}

{ #category : #testing }
RBClassToRename class >> justForTest [
	^ 42
]

{ #category : #performing }
RBClassToRename >> method1 [
	^self method2
]

{ #category : #performing }
RBClassToRename >> method2 [
	^self method1
]
