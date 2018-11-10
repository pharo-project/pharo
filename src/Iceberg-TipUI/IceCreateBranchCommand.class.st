"
I'm a command to create a new branch from a commit. 
this is usefull in the case of commiting changes in unsync repositories (so you open a branch and you commit there)
"
Class {
	#name : #IceCreateBranchCommand,
	#superclass : #IceRepositoryCommand,
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #execute }
IceCreateBranchCommand >> execute [
	| selection |
	selection := UIManager default
		request: 'Please provide the name of the new branch to be created'
		initialAnswer: ''
		title: 'Create new branch'.
	selection isEmptyOrNil ifTrue: [ ^ self ].
	self repository createBranch: selection.
	self beSuccess
]
