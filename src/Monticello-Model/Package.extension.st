Extension { #name : 'Package' }

{ #category : '*Monticello-Model' }
Package >> asMCOrganizationDefinition [

	^ MCOrganizationDefinition packageName: self name tagNames: self tagNames
]
