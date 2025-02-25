"
A MCSystemCategoryParser extracts or set a category/protocol to the corresponding MCOrganizationDefinition with the old definition
"
Class {
	#name : 'MCSystemCategoryParser',
	#superclass : 'MCDoItParser',
	#category : 'Monticello-BackwardCompatibility',
	#package : 'Monticello-BackwardCompatibility'
}

{ #category : 'factory identification hook' }
MCSystemCategoryParser class >> pattern [
	^ 'SystemOrganization*'
]

{ #category : 'actions' }
MCSystemCategoryParser >> addDefinitionsTo: aCollection [

	| definition converter |
	definition := aCollection
		              detect: [ :ea | ea isOrganizationDefinition ]
		              ifNone: [ aCollection add: MCOrganizationDefinition new ].

	converter := CategoryConverter category: self category.
	definition packageName ifNil: [ definition packageName: converter packageName ].
	converter tagName ifNotNil: [ :tagName | definition tagNames: (definition tagNames copyWith: tagName) ]
]

{ #category : 'accessing' }
MCSystemCategoryParser >> category [

	| tokens |
	tokens := OCParser parseLiterals: source.
	tokens size = 3 ifFalse: [
		self error: 'Unrecognized category definition' ].
	^ tokens at: 3
]
