Extension { #name : 'Package' }

{ #category : '*Tool-Base' }
Package >> browse [
	^ (Smalltalk tools toolNamed: #browser) openOnPackage: self
]
