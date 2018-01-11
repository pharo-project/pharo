Extension { #name : #RG2PackageDefinition }

{ #category : #'*Ring2-Compatibility' }
RG2PackageDefinition >> realPackage [

	^ RPackageOrganizer default packageNamed: self name ifAbsent: [ nil ]

]
