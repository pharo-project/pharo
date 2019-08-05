Extension { #name : #RPackage }

{ #category : #'*Deprecated80' }
RPackage >> categoryName [
	self
		deprecated: 'This method will be removed. Clients should call name directly'
		transformWith: '`@receiver categoryName' -> '`@receiver name'.
	^ self name
]
