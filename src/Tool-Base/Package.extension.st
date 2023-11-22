Extension { #name : 'Package' }

{ #category : '*Tool-Base' }
Package >> browse [
	^ Smalltalk tools browser openOnPackage: self
]
