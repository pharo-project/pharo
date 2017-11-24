"
Command to save a package in versionner
"
Class {
	#name : #MBSavePackageCommand,
	#superclass : #MBAbstractPackageCommand,
	#category : #'Versionner-Core-Commands'
}

{ #category : #documentation }
MBSavePackageCommand >> documentation [
	^ 'Save a new version of the mcz for the given repository in its repository.'
]

{ #category : #execute }
MBSavePackageCommand >> execute [
	| message repo gofer |
	message := self uiManager multilineRequest: 'Enter commit message' initialAnswer: ''.
	message == nil
		ifTrue: [ ^ self ].
	repo := self checkUserInRepositoryOfPackage: target spec name.
	gofer := Gofer new.
	gofer disablePackageCache.
	gofer repository: repo.
	gofer package: target spec name.
	gofer commit: message
]

{ #category : #accessing }
MBSavePackageCommand >> title [
	^ 'save'
]
