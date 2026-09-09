"
A MCSystemCategoryParser extracts or set a category/protocol to the corresponding MCOrganizationDefinition with the new definition
"
Class {
	#name : 'MCOrganizationParser',
	#superclass : 'MCDoItParser',
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
		              ifNone: [
		              aCollection add: MCOrganizationDefinition new ].

	tokens := OCParser parseLiterals: source.

	packageName := tokens at: 4.
	tags := tokens at: 6.

	definition
		packageName: packageName;
		tagNames: tags
]
