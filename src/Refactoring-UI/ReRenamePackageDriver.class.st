Class {
	#name : 'ReRenamePackageDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'packageName',
		'newName'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReRenamePackageDriver class >> driverName [

	^ 'Rename package'
]

{ #category : 'configuration' }
ReRenamePackageDriver >> instantiateRefactoring [

	^ (ReRenamePackageRefactoring
		model: model
		rename: packageName
		to: newName)
]

{ #category : 'execution' }
ReRenamePackageDriver >> runRefactoring [

	self configureRefactoring.
	refactoring failedApplicabilityPreconditions ifNotEmpty: [ :conditions |
		self informConditions: conditions.
		^ false ].
	self applyChanges
]

{ #category : 'execution' }
ReRenamePackageDriver >> scopes: refactoringScopes packageName: aName newName: aNewName [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	packageName := aName.
	newName := aNewName
]
