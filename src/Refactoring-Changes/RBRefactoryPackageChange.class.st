"
I am the baseclass for all refactoring changes for all kind of package changes.
"
Class {
	#name : 'RBRefactoryPackageChange',
	#superclass : 'RBRefactoryChange',
	#instVars : [
		'browserEnvironment'
	],
	#category : 'Refactoring-Changes-Changes-Packages',
	#package : 'Refactoring-Changes',
	#tag : 'Changes-Packages'
}

{ #category : 'converting' }
RBRefactoryPackageChange >> asUndoOperation [
	self subclassResponsibility
]

{ #category : 'private' }
RBRefactoryPackageChange >> executeNotifying: aBlock [
	| undo |
	undo := self asUndoOperation.
	undo name: self name.
	self generateChanges.
	aBlock value.
	^ undo
]

{ #category : 'private' }
RBRefactoryPackageChange >> generateChanges [
	self subclassResponsibility
]

{ #category : 'initialization' }
RBRefactoryPackageChange >> initialize [
	super initialize.
	browserEnvironment := RBBrowserEnvironment default
]

{ #category : 'testing' }
RBRefactoryPackageChange >> isInScope: aRBBrowserEnvironment [ 
	^ aRBBrowserEnvironment includesPackage: self
]
