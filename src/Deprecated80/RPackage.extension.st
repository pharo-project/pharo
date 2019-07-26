Extension { #name : #RPackage }

{ #category : #'*Deprecated80' }
RPackage >> categoryName [
	self
		deprecated: 'This method will be removed. Clients should call name directly'.
	^ self name
]
