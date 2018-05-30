Extension { #name : #SmalltalkImage }

{ #category : #'*RPackage-Core' }
SmalltalkImage >> packages [

	^ (RPackageOrganizer default packages) sorted
]
