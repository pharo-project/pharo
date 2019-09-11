Extension { #name : #MCOrganizationDefinition }

{ #category : #'*Ring-Definitions-Monticello' }
MCOrganizationDefinition >> asRingDefinition [

	^RGOrganization new
		categories: self categories;
		yourself  
]
