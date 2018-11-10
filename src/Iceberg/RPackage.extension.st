Extension { #name : #RPackage }

{ #category : #'*Iceberg-Core' }
RPackage >> iceRepository [
	^ Iceberg repositoryForPackage: self

]
