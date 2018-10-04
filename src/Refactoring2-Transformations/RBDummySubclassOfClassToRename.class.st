"
This is a dummy class to test RenameClass transformation
"
Class {
	#name : #RBDummySubclassOfClassToRename,
	#superclass : #RBDummyClassToRename,
	#instVars : [
		'rewriteRule10'
	],
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #accessing }
RBDummySubclassOfClassToRename >> calls [

	^ self rewriteRule10: self name , self rewriteRule10
]

{ #category : #accessing }
RBDummySubclassOfClassToRename >> calls1 [

	^ self rewriteRule10: (self rewriteRule10: self calls)
]

{ #category : #performing }
RBDummySubclassOfClassToRename >> reference [

	^ RBDummyClassToRename new
]

{ #category : #accessing }
RBDummySubclassOfClassToRename >> rewriteRule10 [
	^ rewriteRule10
]

{ #category : #accessing }
RBDummySubclassOfClassToRename >> rewriteRule10: anObject [
	rewriteRule10 := anObject
]

{ #category : #performing }
RBDummySubclassOfClassToRename >> symbolReference [

	^ #RBDummyClassToRename
]
