Extension { #name : #PackageGroup }

{ #category : #'*Nautilus' }
PackageGroup >> restrict: anEnvironment [
	^ anEnvironment forPackages: self packages
]
