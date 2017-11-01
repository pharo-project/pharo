Extension { #name : #MCOrganizationDefinition }

{ #category : #'*Ring-Monticello' }
MCOrganizationDefinition >> asRingDefinition [

	^RGOrganization new
		categories: self categories;
		yourself  
]
