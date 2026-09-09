Class {
	#name : 'ReRenamePackageDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'packageName',
		'newName',
		'shouldEscape'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReRenamePackageDriver class >> driverName [

	^ 'Rename package'
]

{ #category : 'execution' }
ReRenamePackageDriver >> gatherUserInput [
	 
	 shouldEscape := false.
    newName := self requestNewNameBasedOn: packageName.
    shouldEscape ifTrue: [ ^ false ].
    ^ true
]

{ #category : 'configuration' }
ReRenamePackageDriver >> instantiateRefactoring [
    ^ ReRenamePackageRefactoring new
        model: model;
        rename: packageName;
        yourself
]

{ #category : 'private' }
ReRenamePackageDriver >> requestNewNameBasedOn: aName [

    newName := self defaultRequestDialog
        title: 'Rename a package';
        message: 'Enter new package name';
        text: aName;
        openModal.
    newName ifNil: [
        shouldEscape := true.
        ^ self ].
    refactoring newName: newName.
    ^ newName
]

{ #category : 'execution' }
ReRenamePackageDriver >> scopes: refactoringScopes packageName: aName [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	packageName := aName.
]
