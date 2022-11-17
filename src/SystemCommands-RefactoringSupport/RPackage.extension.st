Extension { #name : #RPackage }

{ #category : #'*SystemCommands-RefactoringSupport' }
RPackage class >> scopesIconName [
	^ #package
]

{ #category : #'*SystemCommands-RefactoringSupport' }
RPackage >> subelements [
	^ self classes asArray
]
