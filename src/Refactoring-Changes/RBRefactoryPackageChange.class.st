"
I am the baseclass for all refactoring changes for all kind of package changes.
"
Class {
	#name : #RBRefactoryPackageChange,
	#superclass : #RBRefactoryChange,
	#category : #'Refactoring-Changes'
}

{ #category : #converting }
RBRefactoryPackageChange >> asUndoOperation [
	self subclassResponsibility 
]

{ #category : #private }
RBRefactoryPackageChange >> executeNotifying: aBlock [
	| undo |
	undo := self asUndoOperation.
	undo name: self name.
	self primitiveExecute.
	aBlock value.
	^ undo
]

{ #category : #private }
RBRefactoryPackageChange >> primitiveExecute [
	self subclassResponsibility
]
