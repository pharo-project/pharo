Extension { #name : 'RPackage' }

{ #category : '*Monticello-Model' }
RPackage >> asMCOrganizationDefinition [

	^ MCOrganizationDefinition packageName: self name tagNames: self tagNames
]
