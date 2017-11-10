Extension { #name : #MCFileBasedRepository }

{ #category : #'*MonticelloGUI' }
MCFileBasedRepository >> morphicOpen: aWorkingCopy [
	(MCFileRepositoryInspector repository: self workingCopy: aWorkingCopy)
		show
]
