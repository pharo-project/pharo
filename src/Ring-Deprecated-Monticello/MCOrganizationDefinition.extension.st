Extension { #name : #MCOrganizationDefinition }

{ #category : #'*Ring-Deprecated-Monticello' }
MCOrganizationDefinition >> asRingDefinition [

	^RGOrganization new
		categories: self categories;
		yourself  
]
