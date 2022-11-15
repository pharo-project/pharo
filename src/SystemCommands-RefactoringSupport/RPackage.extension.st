Extension { #name : #RPackage }

{ #category : #'*SystemCommands-RefactoringSupport' }
RPackage >> subelements [
	^ self classes asArray
]
