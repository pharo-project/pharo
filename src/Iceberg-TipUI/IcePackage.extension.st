Extension { #name : #IcePackage }

{ #category : #'*Iceberg-TipUI' }
IcePackage >> browse [
	
	(RPackageOrganizer default packageNamed: self name) browse
]
