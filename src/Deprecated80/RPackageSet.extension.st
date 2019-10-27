Extension { #name : #RPackageSet }

{ #category : #'*Deprecated80' }
RPackageSet >> extensionClasses [
	self deprecated: 'Use #extensionClasses instead.' transformWith: '`@receiver extensionClasses' -> '`@receiver extendedClasses'.
	^ self extendedClasses
]
