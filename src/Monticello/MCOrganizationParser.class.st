"
A MCSystemCategoryParser extracts or set a category/protocol to the corresponding MCOrganizationDefinition with the new definition
"
Class {
	#name : 'MCOrganizationParser',
	#superclass : 'MCSystemCategoryParser',
	#category : 'Monticello-Chunk Format',
	#package : 'Monticello',
	#tag : 'Chunk Format'
}

{ #category : 'factory identification hook' }
MCOrganizationParser class >> pattern [
	^ '*ensurePackage:*'
]

{ #category : 'actions' }
MCOrganizationParser >> addDefinitionsTo: aCollection [

	| definition tokens packageName tags |
	definition := aCollection
		              detect: [ :ea | ea isOrganizationDefinition ]
		              ifNone: [ aCollection add: MCOrganizationDefinition new ].

	tokens := source parseLiterals.

	packageName := '' join: (tokens copyFrom: 4 to: tokens size - 2).
	tags := tokens last.

	definition
		packageName: packageName;
		tagNames: tags
]
