Extension { #name : 'Package' }

{ #category : '*Refactoring-Environment' }
Package >> asEnvironment [
	^ RBPackageEnvironment package: self
]
