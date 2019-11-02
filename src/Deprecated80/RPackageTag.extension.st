Extension { #name : #RPackageTag }

{ #category : #'*Deprecated80' }
RPackageTag >> extensionClasses [ 
	self deprecated: 'Use #extensionClasses instead.' transformWith: '`@receiver extensionClasses' -> '`@receiver extendedClasses'.
	^ self extendedClasses
]
