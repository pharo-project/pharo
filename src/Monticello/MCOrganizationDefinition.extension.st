Extension { #name : 'MCOrganizationDefinition' }

{ #category : '*Monticello' }
MCOrganizationDefinition >> accept: aVisitor [
	^ aVisitor visitOrganizationDefinition: self
]
