"
I am the baseclass for all refactoring changes for all kind of package changes.
"
Class {
	#name : 'RePackageChange',
	#superclass : 'ReChange',
	#instVars : [
		'browserEnvironment'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'converting' }
RePackageChange >> asUndoOperation [
	self subclassResponsibility
]

{ #category : 'displaying' }
RePackageChange >> displayString [

	^ self changeString
]

{ #category : 'private' }
RePackageChange >> executeNotifying: aBlock [
	| undo |
	undo := self asUndoOperation.
	undo name: self name.
	self generateChanges.
	aBlock value.
	^ undo
]

{ #category : 'private' }
RePackageChange >> generateChanges [
	self subclassResponsibility
]

{ #category : 'initialization' }
RePackageChange >> initialize [
	super initialize.
	browserEnvironment := RBBrowserEnvironment default
]

{ #category : 'testing' }
RePackageChange >> isInScope: aRBBrowserEnvironment [ 
	^ aRBBrowserEnvironment includesPackage: self
]
