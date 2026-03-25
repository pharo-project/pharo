Extension { #name : 'Package' }

{ #category : '*Tools' }
Package >> browse [
	^ (Smalltalk tools toolNamed: #browser) openOnPackage: self
]
