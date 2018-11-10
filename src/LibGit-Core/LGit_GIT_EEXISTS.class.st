Class {
	#name : #'LGit_GIT_EEXISTS',
	#superclass : #LGitCallReturnHandler,
	#category : #'LibGit-Core-Exceptions-ReturnCodes'
}

{ #category : #visiting }
LGit_GIT_EEXISTS >> acceptError: aVisitor [

	^ aVisitor visitEEXISTS: self
]
