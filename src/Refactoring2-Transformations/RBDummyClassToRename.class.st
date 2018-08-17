"
This is a dummy class to test RenameClass transformation
"
Class {
	#name : #RBDummyClassToRename,
	#superclass : #Model,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #performing }
RBDummyClassToRename >> method1 [

	^ self method2
]

{ #category : #performing }
RBDummyClassToRename >> method2 [
	
	^ self method1
]
