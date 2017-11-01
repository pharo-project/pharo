Extension { #name : #RPackage }

{ #category : #'*Refactoring-Environment' }
RPackage >> asEnvironment [
	^ RBPackageEnvironment package: self
]
