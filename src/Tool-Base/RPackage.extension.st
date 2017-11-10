Extension { #name : #RPackage }

{ #category : #'*Tool-Base' }
RPackage >> browse [
	^ Smalltalk tools browser openOnPackage: self
]
